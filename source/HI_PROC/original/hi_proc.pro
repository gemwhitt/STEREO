pro hi_proc

; Source Program for processing raw STEREO (level2) data
; Calls: process_hi_data

Compile_opt idl2

opt='a' ; or b

spawn, 'date +%d_%m', today ; get todays date

if opt eq 'b' then begin
  indir='/data9/gemma/stereo_new/'+opt+'/raw/'
  outdir='/data9/gemma/stereo_new/'+opt+'/l2/'
endif else begin
  indir='/data8/gemma/stereo_new/'+opt+'/raw/'
  outdir='/data8/gemma/stereo_new/'+opt+'/l2/'
  endelse

filedir1=file_search(indir+'2011*')  ; e.g. 20100801
;filedir2=file_search(indir+'2011*')  ; e.g. 20100801
filedir=[filedir1]  ;,filedir2]  ; e.g. 20100801

fts_files=file_search(filedir+'/*.fts')  ; ~36 files per day

dates=file_basename(filedir) ; ALL folders in /raw/
ndates=n_elements(filedir)      ; Number of folders        

blocksize=3.0 ; number of dates to be blocked 

startday=dates[0] ; first day to process

if (ndates mod blocksize) eq 0.0 then begin   ; of number of folders is divisible by three then...
endday=file_basename(dates[ndates-1])  ; last day to process
nblocks=floor(ndates/blocksize)
ndates=floor(nblocks*blocksize)      ; new number of dates
endif else begin  ; alter endday to give complete blocks
nblocks=floor(ndates/blocksize)
ndates=floor(nblocks*blocksize)      ; new number of dates
endday=file_basename(dates[ndates-1]) ; altered endday
dates=dates[0:ndates-1]       ; new list of dates
endelse

print, 'Number of days to process: '+strtrim(ndates,2)
print, 'Number of output files will be: '+strtrim(nblocks,2)

files=file_search(indir+dates+'/*.fts')  ; new file list

badpath='/data1/gemma/IDL/HI_PROC/resource/'  ; path to bad files

openw, lun, '/data1/gemma/IDL/HI_PROC/data/processing_info_'+opt+'.dat', /get_lun, /append
printf, lun, 'Processing info for '+strtrim(today,2)
printf, lun, 'startday: '+strtrim(startday,2)
printf, lun, 'endday: '+strtrim(endday,2)
printf, lun, 'initial number of files: '+strtrim(n_elements(files),2)
free_lun, lun

process_hi_data, opt, indir, files, blocksize, startday=startday, endday=endday, $
                     bgchecks=bgchecks, outpath=outdir, badpath = badpath, /checks
                                          

end
