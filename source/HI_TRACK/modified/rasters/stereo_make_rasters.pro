pro stereo_make_rasters

Compile_opt idl2
  
print, systime()
  
yymm='2007'
  
filesin=file_search('~/STEREO/BERT/data/p1/'+yymm+'*.sav')
  
target_list='~/STEREO/BERT/lists/b_tar_list.txt' ;list of targets to search for..
  
; Path where the output is stored
outdir='~/STEREO/BERT/data/rasters/'
  
trackstars_rasters, filesin,target_list,outdir,yymm
  
print,'End of processing'
print, systime()
  
END
