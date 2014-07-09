pro process_hi_data2, indir, files, outdir, procdate
  
;   PURPOSE: To produce L2 science level images which will have photometry performed on them.  

;   CHECKS: Run checks to ensure all images are from the same s/c and HI detector

;   CALLS: prep_hi_data
 
nf=n_elements(files)     ; number of .fts files
  
IF nf eq 0 THEN BEGIN
  print, 'No files passed for processing'
  stop
ENDIF
    
; CHECKS - use headers to check all files are from A/B and HI-1/HI-2 ;;;;;;;;;;  
mreadfits, files, hdrs, /quiet
    
iloc = where((strmatch(hdrs.detector, hdrs[0].detector) eq 1) and $
   (strmatch(hdrs.obsrvtry, hdrs[0].obsrvtry) eq 1), complement=bad)
      
IF bad[0] ne -1 THEN BEGIN
  print, 'Files are not all for the same s/c or detector'
  stop
ENDIF
  
outfile=outdir+procdate+'_p1.sav'

mreadfits,files,index1,data, /quiet

stop

print, 'saving'
save, filename=outfile, data, index1
print, 'saved'
  

END
