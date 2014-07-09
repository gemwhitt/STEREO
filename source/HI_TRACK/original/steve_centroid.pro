FUNCTION steve_centroid,data,imstars,width=width,fwhm=fwhm

dsz = size(data)

x = round(imstars[0,*])
y = round(imstars[1,*])

isz = size(imstars,/dim)

;set up new array to collect results and output
imstars2 = imstars*0.

IF n_elements(isz) gt 1 THEN temp=isz[1] ELSE temp=1 ;so that even a single star can be tracked.

FOR i=0,temp-1 DO BEGIN

  IF (x[i]-width lt 0 or x[i]+width gt dsz[1]-1 or y[i]-width lt 0 or y[i]+width gt dsz[2]-1) THEN BEGIN
    imstars2[*,i] = imstars[*,i]   
 
  ENDIF ELSE BEGIN
    sub = data[x[i]-width:x[i]+width,y[i]-width:y[i]+width]
    
    sz1=size(sub,/dim)
   
    rbin=5.
    
    bigsub = congrid(sub,sz1[0]*rbin,sz1[1]*rbin,/interp)
    
    bigsub2 = filter_image(bigsub,fwhm=fwhm,/all_pixels)  ; was 3.0,8 - Vino
    
    max_bigsub=(where(bigsub2 eq max(bigsub2)))[0]
    subx_guess = (array_indices(bigsub2, max_bigsub))[0]
    suby_guess = (array_indices(bigsub2, max_bigsub))[1]

    gcntrd,bigsub2,subx_guess,suby_guess,xgcntrd3c,ygcntrd3c,fwhm,/silent   ; was 5.,8 -Vino

    IF (xgcntrd3c ne -1 and ygcntrd3c ne -1) THEN BEGIN
      imstars2[0,i] = x[i]-width+xgcntrd3c/rbin
      imstars2[1,i] = y[i]-width+ygcntrd3c/rbin

;     plots,xgcntrd3c,ygcntrd3c,psym=1,color=1
;  wait,2.
    ENDIF ELSE BEGIN
    
      imstars2[*,i] = (array_indices(bigsub2, max_bigsub))
      
  
    ENDELSE
  ENDELSE
ENDFOR

return,imstars2

END
