pro bert_convert_ra

; Purpose: to convert Bert's RA and DEC coords from hh mm ss, dd mm ss to: ddd.ddd, ddd.ddd
; 
; Use template bert_template
; 
Compile_opt idl2

filein='~/STEREO/BERT/berts_targets.txt'

template='~/IDLWorkspace82/BERT/resource/bert_template.sav'

; restore template
restore, template ; bert_template

; read ascii file
data=read_ascii(filein, template=bert_template)

ra1=data.ra       ; in hh mm ss
dec1=data.dec     ; in dd mm ss
names=data.name
mags=data.mag

n=n_elements(names)

; convert coordinates 
ra2=float(strmid(ra1, 0, 2))*15. + float(strmid(ra1, 3, 2))/60.*15 + float(strmid(ra1, 6))/3600.*15

; determine if plus or minus
plmi=strmid(dec1, 0, 1)

dec2=float(strmid(dec1, 1, 3)) + float(strmid(dec1, 4, 2))/60. + float(strmid(dec1, 6))/3600.

for ii=0, n-1 do if plmi[ii] eq '-' then dec2[ii]=dec2[ii]*(-1)

ra_dec=fltarr(2,n)

ra_dec[0,*]=ra2
ra_dec[1,*]=dec2

; print out coords to file
fileout='~/STEREO/BERT/b_tar_list.txt
openw, lun, fileout, /get_lun
for ii=0, n-1 do printf, lun, names[ii], mags[ii], ra_dec[0,ii], ra_dec[1,ii], format='(a10,x,f,x,f,x,f)'
free_lun, lun



print, 'End of program'
end