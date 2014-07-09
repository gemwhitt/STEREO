pro hi_track_bert1

Compile_opt idl2

print, systime()
  
yymm='2007'
  
filesin=file_search('~/STEREO/BERT/data/p1/'+yymm+'*.sav')

target_list='~/STEREO/BERT/lists/b_tar_list.txt' ;list of targets to search for..
  
; Path where the output is stored
outpath='~/STEREO/BERT/data/p2/'+yymm
  
track_stars3_bert1, filesin,target_list,outpath
  
print,'End of processing'
print, systime()

END
