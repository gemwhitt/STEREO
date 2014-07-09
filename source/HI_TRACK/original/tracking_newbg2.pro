PRO tracking_newbg2

; Need : Source file to track stars using the new background developed by RAL.
; History : 30 Mar 2010
; Calls : track_stars3 and subroutines therein.
; Restrictions :  Needs the ra, dec and mag values of the catalogues to be stored
; as a .sav file named 'radec_mag.sav' in the folder resource.
; The final output is stored as a .sav file in the directory of the data itself
; It is recommended to process month by month due to memory constraints. 

t = systime(1)

CD,current=dir
;Gets the L2 data files
;datapath=file_search('/data3/vs/L2*/*/*L2.sav') ;all images from mar07 till apr08
datapath = file_search(filepath('*L2.sav',SUBDIR=["data","*"],ROOT_DIR=dir));specifically for cluster


; Gets the catalogue information
;catalogue_files=filepath('radec_mag.sav',SUBDIR=['resource'],ROOT_DIR=dir)
catalogue_files=filepath('radec_mag_new.sav',SUBDIR=['resource'],ROOT_DIR=dir) ;the actual complete catalogue
; Path where the output is stored
;outpath=file_dirname(files[0])+'/'

;files=file_search(datapath)
;restore,datapath

restore,file_search(catalogue_files)


;;consider only mag gt 9.
;ind = where(mag gt 10. and mag lt 10.5)
;ra = ra[ind]
;dec = dec[ind]
;mag = mag[ind]

print, 'Total Stars in Catalogue : '+strcompress(string(n_elements(ra)), /rem)

track_stars3,datapath,ra,dec,mag,/display;,outpath;

print,'End of processing'
print,'Time taken = ',systime(1)-t
END