pro hi_proc_bert1

; Source Program for processing raw STEREO (level2) data - extract fits files, throw away bad ones and put in .sav file
; Calls: process_hi_data2
; Modified from hi_proc on 2nd July
  
Compile_opt idl2

procdate='2007' ; give the program a date, e.g. 201008

indir='/Users/gemmawhittaker/STEREO/BERT/data/l2_1d/'
outdir='/Users/gemmawhittaker/STEREO/BERT/data/p1/'

filedir=file_search(indir+procdate+'*')  ; e.g. 20100801  ; ~30 days in a month
    
files=file_search(filedir+'/*.fts', count=nf)  ; ~36 files per day  - list of all fits files to be processed
  
dates=file_basename(filedir) ; ALL folders in /raw/
nd=n_elements(filedir)      ; Number of folders
  
print, 'Processing'
print, 'startday: '+strtrim(dates[0],2)
print, 'endday: '+strtrim(dates[nd-1],2)
print, 'initial number of files: '+strtrim(nf,2)
  
process_hi_data2, indir, files, outdir, procdate
    
end
