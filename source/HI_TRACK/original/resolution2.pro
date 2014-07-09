pro resolution2,vfov1,vfov2

Compile_Opt defint32

g=0   ; counting variable - need to run the entire loop twice to avoid missing close neighbours
sz1=0 ; define for the purposes of the next line in the program to work

REPEAT BEGIN  ; LOOP 1 - end when g=2, i.e. after two passes

  if g ge 0 AND sz1 eq n_elements(vfov1)/3 then goto,finish   

  sz1=n_elements(vfov1)/3                ; size of this vfov1 array
  print, 'g '+strtrim(g+1,2)+' - size of vfov1 is '+strtrim(sz1,2)   
  
  ; Set up starting parameters 
  start=0
  limit=6000
  pos=0                  

  REPEAT BEGIN  ; LOOP 2 - end when endd=sz, i.e. at end of vfov1 array

    endd=start+limit

    if endd gt sz1 then endd=sz1      ; endd should not exceed sz 

    ;initialise arrays
    x=vfov1[0,start:endd-1]
    x=x[*]
    y=vfov1[1,start:endd-1]
    y=y[*]
    mag=vfov1[2,start:endd-1]

    this_size=endd-start              ; size of this sample of stars
    temp=intarr(this_size)+(99)
    
    ;calculate the distance array
    dist=sqrt((rebin(transpose(x),this_size,this_size)-rebin(x,this_size,this_size))^2+(rebin(transpose(y),this_size,this_size)-rebin(y,this_size,this_size))^2)

    i = 1l

    REPEAT BEGIN  ; LOOP 3 - end when ncount=0
  
      ;find those objects whose distances are within 6 pixels 
      result = histogram(reform(dist[i-1,*]),binsize=6.,reverse_indices=ri) 

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
 
      temp[i-1] = 10 ; mark the checked object
      faint: count = where(temp gt 15,ncount)
      i = i+1 ; check the next object
      
    ENDREP UNTIL ncount eq 0    ; END LOOP 3
   ; print, 'i='+strtrim(i,2)
  
    rsl=where(temp gt 1 and temp lt 98)
    rsl=rsl+start	; correct the index position to correspond with main array
   
    n_rsl=n_elements(rsl)   ; number of remaining stars from this sample
        
    vfov1[0,pos]=vfov1[0,rsl]
    vfov1[1,pos]=vfov1[1,rsl]
    vfov1[2,pos]=vfov1[2,rsl]
    
    vfov2[0,pos]=vfov2[0,rsl]
    vfov2[1,pos]=vfov2[1,rsl]
    
    pos=pos+n_rsl
    
     ;re-establish starting parameters - for next loop
    start=endd

  ENDREP UNTIL endd eq sz1     ; END LOOP 2
   
  vfov1=vfov1[*,0:pos-1]
  
  vfov2=vfov2[*,0:pos-1]

  g=g+1

ENDREP UNTIL limit ge sz1     ; END LOOP 1

finish: print, 'Skipped out of loop as no more stars can be deleted from field'

print, 'number of catalogue stars for tracking: '+strtrim(pos,2)

print, 'minimum x-value of cat stars: '+strtrim(min(vfov1[0,*]),2)  ; for check
print, 'maximum x-value of cat stars: '+strtrim(max(vfov1[0,*]),2)  ; for check

;print, systime()
;stop
END


