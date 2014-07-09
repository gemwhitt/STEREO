pro process_hi_data, opt, indir, files, blocksize, startday=startday, endday=endday, checks=checks, $
                     bgchecks = bgchecks, outpath=outpath, badpath = badpath

;+
;   NAME:
;     PROCESS_HI_DATA
;
;   PURPOSE:
;     To produce L2 science level images which will have
;     photometry performed on them.  Data will be stored
;     in blocks of up to 3 days in length.
;
;   INPUTS:
;     FILES - list of files to be processed.  These need be
;             consecutive in time and also all be from the 
;             same instrument.
;
;     BLOCKSIZE - how many days worth of data to store in a data
;                 block
;
;   OUTPUTS:
;     outputs IDL save files containing the cleaned images
;
;   KEYWORDS:
;     STARTDAY - first day in block of files to process in the STEREO naming
;                format e.g. 20070423 for 23rd April 2007
;     ENDDAY   - last day in block of files to process
;     CHECKS   - run checks to ensure all images are from the same  
;                s/c and HI detector
;   CALLS:
;     prep_hi_data
;
;   HISTORY:
;     original - 12-Nov-07 - Steve Spreckley
;              - 12-Feb-07 - Steve Spreckley - robustified and modified to
;                            handle any number of files seamlessly.
;   NOTES:
;     Due to memory constraints no more than a few days should be 
;     saved into a block.  I have used 3 day blocks typically in the past.
;     It is recommended that blocks are passed such that complete
;     days are kept together (for bg removal)
;
;     Also - if you specify a number of days per output data block then
;     ensure the input block contains an integer multiple no. of days
;     of data else at least one block will be short.
;-

;IF not keyword_set(outpath) THEN outpath='/data1/gemma/HI_PROC/data/track_in_a'
;IF not keyword_set(badpath) THEN badpath='/data1/gemma/HI_PROC/resource/'

   ;  CHECK THAT ALL OF THE FILES ARE FROM THE SAME S/C + INSTRUMENT
   ;  BY CHECKING HEADER KEYWORDS

sz1 = size(files, /dim)   ; number of .fts files

IF sz1[0] eq 0 THEN BEGIN
   print, 'No files passed for processing'
   stop
ENDIF


;;;;;;;;;; CHECKS - use headers to check all files are from A/B and HI-1/HI-2 ;;;;;;;;;;
IF keyword_set(checks) THEN BEGIN

  mreadfits, files, hdrs, /quiet
  
  if opt eq 'a' AND hdrs[0].obsrvtry ne 'STEREO_A' then begin
    print, 'observatory mismatch, opt='+opt+', obs='+hdrs[0].obsrvtry
    stop
  endif
  
  if opt eq 'b' AND hdrs[0].obsrvtry ne 'STEREO_B' then begin
    print, 'observatory mismatch, opt='+opt+', obs='+hdrs[0].obsrvtry
    stop
  endif
  

  iloc = where((strmatch(hdrs.detector, hdrs[0].detector) eq 1) and $
         (strmatch(hdrs.obsrvtry, hdrs[0].obsrvtry) eq 1), complement=bad)
  
  IF bad[0] ne -1 THEN BEGIN
     print, 'Files are not all for the same s/c or detector'
     return
  ENDIF
ENDIF ELSE BEGIN
   mreadfits, files[0], hdrs, /quiet
  ;read_hdrs,hdrs    ; read in one header anyway as we need it
ENDELSE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;--------------- SETUP WHERE TO START/END THE PROCESSING ------------
fbase = file_basename(files)
dates = strmid(fbase, 0, 8)
firstday = dates[0]
lastday = dates[sz1-1]

IF not keyword_set(startday) THEN BEGIN
   startday = firstday & i0 = 0
   startdays = strcompress(string(startday), /remove)
ENDIF ELSE BEGIN
   startdays = strcompress(string(startday), /remove)
   i0 = (where(dates eq startdays))[0]
ENDELSE
IF not keyword_set(endday) THEN BEGIN
   endday = lastday & i1 = sz1[0]-1
   enddays = strcompress(string(endday), /remove)
ENDIF ELSE BEGIN
   enddays = strcompress(string(endday), /remove)
   i1 = (where(dates eq enddays))
   ;i1 = reform(i1[n_elements(i1)-1])-1 ; Vino's original code - seems to miss out last element in files
   i1 = reform(i1[n_elements(i1)-1])    ; Modified 5/4/2011 to include last element in files
ENDELSE

files = files[i0:i1]
fbase = fbase[i0:i1]
dates = dates[i0:i1]
sz1 = i1-i0+1

print, 'Beginning to process '+strcompress(string(sz1), /rem)+' files'

;------------- Read in the relevant bad image file -------------------  

;HI1/HI2 detector // A/B platform
sc_ab = strlowcase(strmid(hdrs[0].obsrvtry,7,1))
sc_hi = strlowcase(hdrs[0].detector)
baddata_file = badpath+sc_hi+sc_ab+'_bad.txt'
openr, lun, /get, baddata_file
nbad = file_lines(baddata_file)
baddata = strarr(2, nbad)
readf, lun, baddata, format='(A25, 1X, A3)'
free_lun, lun

;------------- Now begin to process the files -------------------------

; remove files matching those in the bad file data
; use for loop as there are only a few files

FOR i = 0, nbad-1 do begin
   badloc = where(fbase eq baddata[0,i], nbad)  
   IF nbad gt 0 THEN  BEGIN
      print, 'Bad file found: '+files[badloc]
      IF badloc eq 0 THEN BEGIN
         files = files[1:sz1-1]
         fbase = fbase[1:sz1-1]
      ENDIF ELSE IF badloc eq sz1-1 THEN BEGIN
         files = files[0:sz1-2]
         fbase = fbase[0:sz1-2]
      ENDIF ELSE BEGIN
         files = [files[0:badloc-1], files[badloc+1:sz1-1]]
         fbase = [fbase[0:badloc-1], fbase[badloc+1:sz1-1]]
      ENDELSE
      sz1-=1    ; subtract 1 off the number of elements to process
   ENDIF
ENDFOR

print, 'There are '+strcompress(string(sz1), /rem)+' files left after bad file removal'

openw, lun, '/data1/gemma/IDL/HI_PROC/data/processing_info_'+opt+'.dat', /get_lun, /append
printf, lun, 'number of good files: '+strtrim(sz1, 2)
free_lun, lun


;IF keyword_set(bgchecks) THEN bgchecks,files,baddata_list = baddata_list,size = sz1,/files ; run a crude bad image check (needs improving)
IF keyword_set(bgchecks) THEN BEGIN
  npb = 300.
  nblocks = ceil(sz1/npb)
  print, 'Using'+string(nblocks)+' blocks for bg check'
  goodvals = intarr(sz1)
  FOR i = 0, nblocks-1 DO BEGIN
     lowf = i*fix(npb)
     highf = ((i+1)*fix(npb) - 1) < (sz1-1)
     nf = highf-lowf+1
     ;print, lowf, highf, nf

     
     fileblock = files[lowf:highf]
     mreadfits, fileblock, hdr2, data1, /quiet
     
     maxx = fltarr(nf)
     minn = fltarr(nf)
     FOR kk = 0, nf-1 DO maxx[kk] = max(data1[*,*,kk])
     FOR kk = 0, nf-1 DO minn[kk] = min(data1[*,*,kk])

     sig = robust_sigma(maxx)
     
     IF sig ne 0 THEN BEGIN
          ;modified - vs - 17 09 09 - to make sure that all the bad frames are removed -changed again since it removes most of the data
         good = maxx lt median(maxx)+5*sig  and maxx gt median(maxx)-5*sig and minn eq median(minn)
         
     ENDIF ELSE BEGIN
         good = maxx eq median(maxx) and minn eq median(minn)
     ENDELSE
     
     goodvals[lowf:highf] = good
     data1 = 0      ; clear out some memory
     hdr2 = 0      ; clear out some memory      
  ENDFOR
  good_data = where(goodvals eq 1,complement=bad_data)
   
  ;add files of badframes found to the bad files text - vs -17 09 09
;  IF bad_data(0) ne -1 THEN BEGIN
;     
;      bfile_name=file_basename(files(bad_data))     
;      openw, lun, /get, baddata_list,/append
;      printf,lun,bfile_name+'   '+'bad'
;      close
;      free_lun,lun
;      print,'Bad file found (bg checks): ', bfile_name , 'and added to ',baddata_list
;
;  ENDIF
;  files = files[good_data]
ENDIF  

; recompute size of data block with bad frames removed
fbase = file_basename(files)
sz1 = size(fbase, /dim)
dates = strmid(fbase, 0, 8)
daybreaks = [-1,uniq(dates)]
days = dates[daybreaks]
ndays = n_elements(uniq(days))*1.D
nblocks = floor(ndays/blocksize)
print, 'Splitting into '+strcompress(string(nblocks), /rem)+' blocks'
IF nblocks eq 0 THEN BEGIN
   print, 'Not enough data to fill required block size'
   return
ENDIF


print, 'starting with file '+files[0]
print, 'and finishing with '+files[sz1-1]
;print,'Processing ',n_elements(files),' Files with secchi_prep'

bad_index=-1 ; use this to flag bad frames in a block
FOR i = 0, nblocks-1 DO BEGIN
    lowf = daybreaks[i*blocksize]+1
    highf = daybreaks[(i+1)*blocksize < (n_elements(daybreaks)-1)]
    fblock = files[lowf:highf]
    
    if opt eq 'a' then outfile=outpath+dates[lowf]+'_'+dates[highf]+'_L2a.sav' else $  ; for A
    outfile=outpath+dates[lowf]+'_'+dates[highf]+'_L2b.sav'   ; for B
    
    print, 'Making file: '+outfile
    ;comment this line out if not doing secchi_prep
    
    data = prep_hi_data(fblock, index=index1,/skipsecchi)    ; currently using daily bg mins,data = prep_hi_data(fblock, index=index1,/bkgd)
    ;print, 'saving'
    save, filename=outfile, data, index1, bad_index
    ;print, 'saved'
    
    if opt eq 'a' then postdir='/data8/gemma/stereo_new/'+opt+'/raw_processed/' else $
    postdir='/data9/gemma/stereo_new/'+opt+'/raw_processed/'
    
    ;spawn, 'mv '+indir+strtrim(dates[daybreaks[i*blocksize]+1],2)+' '+postdir 
    ;spawn, 'mv '+indir+strtrim(long(dates[daybreaks[i*blocksize+1]+1]),2)+' '+postdir
    ;spawn, 'mv '+indir+strtrim(dates[daybreaks[i*blocksize+2]+1],2)+' '+postdir
     
ENDFOR

END
