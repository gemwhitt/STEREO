PRO HI_TRACK

; SOURCE FILE to track stars using the new background developed by RAL.
; Modified from tracking_newbg.pro : 06 Jun 2011
;
; Calls: track_stars3 and subroutines therein (requires ssw library).
; Resources:  'resource/radec_mag.sav' - contains the ra, dec and mag values of the catalogued stars.
; 
; The final output is stored as a .sav file in the directory of the data itself
; It is recommended to process month by month due to memory constraints. 

;t = systime(1)
print, systime()

opt='a'
yymm='200811'

;CD, '/data1/gemma/HI_TRACK/', current=dir   ; needed if working from command line
CD, current=dir   ; use if HI_TRACK is set a current working directory

;if opt eq 'a' then datapath = file_search(filepath('201010*L2a.sav',SUBDIR=["data","a"],ROOT_DIR=dir)) else $ ;for A
;datapath = file_search(filepath('201103*L2b.sav',SUBDIR=["data","b"],ROOT_DIR=dir))  ;for B

datapath = file_search(filepath(yymm+'*.sav',SUBDIR=["data",opt],ROOT_DIR=dir))

print, datapath[0]

; Gets the catalogue information
catalogue_files=filepath('radec_mag_red_trim.sav',SUBDIR=['resource','catalogue'],ROOT_DIR=dir)  ;the complete catalogue

; Path where the output is stored 
outpath='/data5/gemma/results/thesis/figs/getlcs/'

track_stars3_070113, dir, datapath,catalogue_files,outpath, opt;, /display 

print,'End of processing'
;print,'Time taken = ',systime(1)-t
print, systime()
END
