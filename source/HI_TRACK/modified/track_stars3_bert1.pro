PRO track_stars3_bert1, data_files,target_list,outpath

; Calls: stars2fov, resolution2, hi_findpeaks2, find_uniq_objects, matchstars,
;        steve_centroid, plot_image, aper, sort_stars, anytim2jd
;

Compile_Opt idl2 ; this makes all the values intrinsically long
  
IF n_params() lt 2 THEN BEGIN
  printf, 'please enter an (array of) data files, ras, decs, and mags'
  return
END

nf=n_elements(data_files)

readcol, target_list, name, mag, ra, dec, format='a,f,d,d'
  
FOR kk = 0, nf-1 DO BEGIN                                     ; BEGIN LOOP OVER MAIN .SAV FILE
    
  print, 'Processing '+data_files[kk]
    
  ;;;set up counter to count frame numbers
  count = -1
  catch_stars = -1
  j=-1
  
  restore, data_files[kk] ; data, index1
    
  index = index1
  inst = 0
  
  ; SET UP PARAMETERS.....
  med_imgs=median(data)
  
  thresh = 3.5  ; threshold for finding stars in data
  
  phpadu = 14.5/0.9 ; (approximate) gain (adu to electron conversion) - from brewscher et al 2010, p13
  
  apr = 2.0 ;[2.0,2.5,3.0] ; aperture radii (pixels)
  
  skyrad = [4.0,7.0]  ; sky aperture inner/outer annulus (pixels)
  
  pix_err = 1.0 ; pixel error between theoretical and observed position of star
  
  nf=n_elements(data_files) ; number of .sav files - usually 1 .sav file = 1 month
    
  dsz = size(data, /dim)  ; e.g. 1024, 1024, 139
   
  data = data*4. ; to make sure that the flux is summed rather averaged    
    
  FOR i=0, dsz[2]-1 DO BEGIN                                  ; BEGIN LOOP OVER IMAGE
    
    count = count+1
    print,'Frame number',count,' j=',j
    
    ;convert catalogue ra and dec coords to x,y positions on CCD
    vfov=stars2fov_bert1(ra,dec,mag,name,index[i],ccd_offsetx=0.,ccd_offsety=0.,vfov2=vfov2,frad=1.,names)

    IF (vfov[0] NE -999) THEN BEGIN                           ; IF TARGET STARS ARE WITHIN FOV
      
      sz=float(n_elements(vfov))/3. ; number of target_list stars in this FOV
        
      ;find peaks/stars in image
      pks=hi_findpeaks_bert1(data[*,*,i],minthresh = thresh)
        
      uniq_pks = find_uniq_objects(pks,ind = pks_ind)
        
      ;match stars to peaks
      idx = matchstars(uniq_pks,vfov) ; give the closest match - but could still be very far away1!
        
      imstars = uniq_pks[*,idx]
        
      ;if theoretical and observed star location too different ignore stars
      ii = where(abs(imstars[0,*]-vfov[0,*]) LT pix_err AND abs(imstars[1,*]-vfov[1,*] LT pix_err), nii)
        
      IF (ii[0] NE -1) THEN BEGIN                             ; IF TARGET STARS MATCH PEAKS    
          
        ; calculate success rate of matches
        ; sz = initial number of targets in the FOV
        ; nii = matched number of targets in FOV
        pmat=float(nii)/float(sz)*100.
        print, 'Percentage of stars matched in FOV=',pmat
        
        imstars = imstars[*,ii]
        vfov = vfov[*,ii]
        vfov2 = vfov2[*,ii]
        names=names[ii]
          
        catch_stars = catch_stars+1
        j=j+1
          
        imstars2 = steve_centroid(data[*,*,i],imstars,width=2,fwhm=1.5) ;  actual values found from the HI paper -1.6
          
        isz = size(imstars2)
          
        ;do aperture photometry on the stars
        aper_bert1,data[*,*,i],imstars2[0,*],imstars2[1,*],flux,errap,sky,skyerr,phpadu,apr,/silent,skyrad,/nan,$
          /flux,/exact
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
              
      ENDIF                                     ; IF TARGET STARS MATCH PEAKS
    ENDIF ELSE BEGIN                            ; IF TARGET STARS ARE WITHIN FOV 
      print,'No stars found'                    ; IF TARGET STARS ARE NOT WITHIN FOV
    ENDELSE                                     ; IF TARGET STARS ARE NOT WITHIN FOV
      
  ENDFOR                                                       ; END LOOP OVER IMAGE
  ; reset counters
  j=-1
  catch_stars=-1
ENDFOR                                                        ; END LOOP OVER MAIN .SAV FILE
  
END
