;+
;
; Name: prep_hi_data
;
; Purpose: Prepares HI data for star tracking
;
; Syntax: data = prep_hi_data(files,index=index,/bkgd)
;
; Inputs: files - list of files which you want to prepare
;
; Outputs: data - data cube of prepared HI data
;          index - array of header structures
;
; Keywords: bkgd - uses min_filter to remove f-corona
;
; Calls: secchi_prep, min_filter, rm_bloom_v2
;
; Restrictions: File list must be from the same spacecraft and same
;               camera - cannot mix and match data from different HI cameras
;
; History: 24-Oct-2007, Danielle Bewsher
;          30-Oct-2007, added extra functionality, DB
;           3-Nov-2007, Steve Spreckley - corrected a number of coding errors
; Contact: Danielle Bewsher (d.bewsher@rl.ac.uk)
;
;-
FUNCTION prep_hi_data,files,index=index,bkgd=bkgd, skipsecchi=skipsecchi

IF (n_elements(files) eq 0) THEN print,'No files given'

IF not keyword_set(skipsecchi) THEN secchi_prep,files,index,data $
ELSE mreadfits,files,index,data, /quiet

;print,'Doing Background subtraction!'
dsz = size(data, /dim)

IF KEYWORD_SET(bkgd) THEN BEGIN
   
  mask_data = data*0.
  ;work out how many days worth of data you want to prepare
  dat = strmid(index.date_d$avg,0,10)
  s_dat = dat(sort(dat))
  u_dat = s_dat(uniq(s_dat))
  ndat = n_elements(u_dat)

  ;set up temporary array to collect blooming mask
  
  ;work on one days worth of data at at time
  ;background subtraction not perfect - but works best on 
  ;one days worth of data at a time
  FOR j=0,ndat-1,2 DO BEGIN                               ;Original line in code - error:Attempt to subscript U_DAT with <INT      (       1)> is out of range.
  ;FOR j=0,ndat-1 DO BEGIN                                 ;Modification made to correct error on 23/03/2011
  
    ii = where(dat eq u_dat(j) or dat eq u_dat(j+1),nii)  ;make one day masks
    IF (nii eq 1) THEN BEGIN
      print,'Can not do background subtraction on one image'
    ENDIF ELSE BEGIN

      data_tmp = data[*,*,ii]
      index_tmp = index[ii]
      files_tmp = files[ii]
      dsz2 = size(data_tmp)
      
      ;identify saturated pixels and return mask 
;      FOR i=0,dsz2(3)-1 DO BEGIN
;        dtmp = rm_bloom_v2(files_tmp(i),index_tmp(i),data_tmp(*,*,i),0,mask_secchi=tmp)
;        mask_data[*,*,i] = tmp 
;      ENDFOR
      ;apply mask to copy of data so that saturated pixels don't muck up
      ;background subtraction
      tmp = data_tmp;*0.0

     ; FOR i=0,dsz2(3)-1 DO tmp(*,*,i) = data_tmp(*,*,i)*mask_data[*,*,i] 
      ;a=n_elements(where(dat eq u_dat(j)))
      ;calculate background subtraction
      IF size(bkgd, /dim) ne 2 then begin
         data_tmp = min_filter1(tmp,base=base)
         print, u_dat(j)
      ;remove background from data
      ;FOR i=0,dsz2(3)-1 DO data_tmp(*,*,i) = data_tmp(*,*,i)-bkgd
      ENDIF
    ENDELSE
      data[*,*,ii] = data_tmp
  ENDFOR
ENDIF
;stop
return,data

END
