pro stereo_make_cutout, outdir, yymm, index, imdata, names, vfov, vfov2, imstars

Compile_opt idl2

imstars=round(imstars)

ncut=n_elements(names)  ; number of cutouts to make

mindist=5   ; minimum distance from edge of the CCD frame
radius=15.  ; radius of box for cutout 

; get image dimensions
imsz=size(imdata, /dim)

for co=0, ncut-1 do begin
  
  ; establish filename using names
  fout=outdir+yymm+'_'+names[co]+'*.sav'
  
  ; check for exsiting fout
  chk=file_search(fout, count=nchk)
  
  x0=imstars[0,co]
  y0=imstars[1,co]
  
  if x0-mindist lt 0 OR y0-mindist lt 0 OR x0+mindist gt imsz[0]-1 OR y0+mindist gt imsz[1]-1 then begin
    print, 'Target is too close to edge of CCD'
    continue
  endif
  
  ; form a cutout of 40x40 pixels = 1600 total pixels
    ; if x0/y0 is within 40 pixels of edge of CCD frame - then buffer the edges of the array with zeros
    
  temparr=fltarr(radius*2,radius*2)
  
  tmpsz=size(temparr, /dim)
  
  ; calculate distance between the bottom left and top right corners IF NECESSARY
  x1=x0-radius & x2=x0+radius & y1=y0-radius & y2=y0+radius
  
  if x1 lt 0 AND y1 lt 0 then $                     ; x2 and y2 = good
    temparr[abs(x1)-1:2*tmpsz[0]-1,abs(y1)-1:2*tmpsz[1]-1]=imdata[0:x2,0:y2]
    
  if x1 lt 0 AND y1 gt 0 then begin                 ; y2 could be good or > imsz[1]-1
    if y2 lt imsz[1]-1 then $                       ; y2 is good
      temparr[abs(x1):tmpsz[0]-1,*]=imdata[0:x2-1,y1:y2-1] else $ ; y2 gt imsz[1]-1
      temparr[abs(x1):tmpsz[0]-1,0:(y2-imsz[1]-1)]=imdata[0:x2-1,y1:imsz[1]-1] 
  endif 
  
  if x1 gt 0 AND x2 lt imsz[0]-1 then begin
    stop
    if y1 lt 0 then $ ; y2 is good
      temparr[*,abs(y1)-1:2*radius-1]=imdata[x1-1:x2-1,0:y2-1] else begin $ ; y1 gt 0 
      if y2 lt imsz[1]-1 then temparr[*,*]=imdata[x1-1:x2-1,y1-1:y2-1] else $
                              temparr[*,0:y2-imsz[1]-1]=imdata[x1-1:x2-1,y1:imsz[1]-1]   
      endelse 
  endif
  
  if x1 gt 0 AND x2 gt imsz[0]-1 then begin
    stop
    if y1 lt 0 then $ ; y2 is good
      temparr[x1:x2-imsz[0]-1,abs(y1)-1:2*radius-1]=imdata[x1:imsz[0]-1,0:y2] else begin $ 
      if y2 lt imsz[1]-1 then temparr[x1:x2-imsz[0]-1,*]=imdata[x1:imsz[0]-1,y1:y2] else $
      temparr[x1:x2-imsz[0]-1:y2-imsz[1]-1]=imdata[x1:imsz[0]-1:x2,y1:imsz[1]-1]
    endelse
  endif
 
 
  plot_image, bytscl(temparr, 0, 10) 
  
  oplot, [x0-x1], [y0-y1], color=cgcolor('orange'), psym=2
  
  
  stop
  
  
  ; FOR THE NUMBER OF TARGERS WITH PHOTOMETRY - APPEND THE .TXT FILES
  tim = index[i].date_d$obs
  sun_time = index[i].sun_time
  tim_jd = anytim2jd(tim)
  time = (tim_jd.int+tim_jd.frac)-(sun_time/(3600.*24))
  
  for sav=0, nii-1 do begin               ; LOOP OVER NUMBER OF STARS WITH PHOT
  
    outfile=outpath+'_'+names[sav]+'.txt'
    star_mag = reform(vfov[2,sav])
    star_radec = reform(vfov2[0:1,sav])
    star_xy = reform(imstars2[0:1,sav])
    star_int = reform(flux[0,sav])
    star_err = reform(errap[0:sav])
    bkgd = sky[sav]
    bkgd_err = skyerr[sav]
    
    openw, lun, /get_lun, /append, outfile
    printf, lun, time, star_int, star_err, bkgd, bkgd_err, $
      star_radec[0], star_radec[1], star_xy[0], star_xy[1], star_mag, $
      format='(d,x,f,x,f,x,f,x,f,x,d,x,d,x,f,x,f,x,f)'
    free_lun, lun
    stop
  endfor                                  ; LOOP OVER NUMBER OF STARS WITH PHOT

  
endfor




end