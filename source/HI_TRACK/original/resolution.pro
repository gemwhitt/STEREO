FUNCTION resolution,vfov1,rsl
;+
;
; Name: resolution
;
; Purpose: To remove stars which are nearby with a magnitude criteria
;
; Syntax: 
;
; Example: rsl = resolution(vfov1)
;
; Inputs: vfov1 - A 3 D array containing the position of stars in pixels and its magnitude
;
; Calls: undefine
;
; Restrictions: 
;
; History: vino feb 08
; modified on 08 July 2010 - complete vectorisation for the distance measurement
;             06 Sep 2010 - this failed when tried with a large number of stars and hence a moderate vectorisation is attempted.
;
; Contact: Vino Sangaralingam -vs@star.sr.bham.ac.uk


;initialise arrays

x=reform(vfov1(0,*))
y=reform(vfov1(1,*))
mag=reform(vfov1(2,*))
sz=size(x,/dim)
print, 'sz ='+strtrim(sz,2)

temp=intarr(sz(0))+(99)
;to save memory
undefine,vfov1

;calculate the distance array
dist=sqrt((rebin(transpose(x),sz[0],sz[0])-rebin(x,sz[0],sz[0]))^2+(rebin(transpose(y),sz[0],sz[0])-rebin(y,sz[0],sz[0]))^2)


i = 1l
  ;stop
  REPEAT BEGIN
  
  ;find those objects whose distances are within 8 pixels   ; TEST!! CHANGE 8 TO 6
  result = histogram(reform(dist[i-1,*]),binsize=6.,reverse_indices=ri)
  ;;result = histogram(reform(sep[i-1,*]),binsize=8.,reverse_indices=ri)  ; to match inserted code

  ;Collect those objects in the first bin - i.e, the closest
  ind = ri[ri[0]:ri[1]-1]
    
  ;Find the magnitude difference between these closest neighbours only
  mag_diff = mag[i-1]-mag[ind]
  
  ;Find those objects to be removed
  bad = where(abs(mag_diff) lt 1.5,nbad)
  
  IF nbad gt 1. THEN BEGIN ; mark the bad object
  
  b1 = where(abs(mag_diff) lt 0.5,nb1)

  IF nb1 gt 1 THEN BEGIN ; remove both the objects if their mag diff is less than 0.5 units
  temp[ind[b1]] = 0 &  temp[i-1] = 0 & goto,faint
  
  ENDIF ELSE temp[ind[bad]] = 0.
  
  
  ENDIF
  ;plot,dist[ind[bad]],mag_diff[bad],psym=1,xrange=[0,10],yrange=[0,2.]
  
  temp[i-1] = 10 ; mark the checked object
  faint: count = where(temp gt 15,ncount)
  i = i+1 ; check the next object
  ENDREP UNTIL ncount eq 0

  
  rsl=where(temp gt 1 and temp lt 98)

  print, 'num rsl '+strtrim(n_elements(rsl),2)
  
   
;  histoplot,reform(vfov[2,*]),binsize=bins,/line_fill,ori=[45,-45],xtitle='Magnitude',title='Magnitude after mag-dist check'
;  device,/close
;  set_plot,'X'
 return,rsl
END














