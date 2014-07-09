PRO trackstars_rasters, data_files,target_list,outdir,yymm

; Calls: stars2fov, resolution2, hi_findpeaks2, find_uniq_objects, matchstars,
;        steve_centroid, plot_image, aper, sort_stars, anytim2jd
;

Compile_Opt idl2 ; this makes all the values intrinsically long

nf=n_elements(data_files)

readcol, target_list, name, mag, ra, dec, format='a,f,d,d'

FOR kk = 0, nf-1 DO BEGIN                                     ; BEGIN LOOP OVER MAIN .SAV FILE

  print, 'Processing '+data_files[kk]
  
  ;;;set up counter to count frame numbers
  count = -1
  catch_stars = -1
  
  restore, data_files[kk] ; data, index1
  
  dsz = size(data, /dim)  ; e.g. 1024, 1024, 139
  
  data = data*4. ; to make sure that the flux is summed rather averaged
  
  index = index1
  ;inst = 0                     DO WE NEED THIS? 
  
  ; SET UP PARAMETERS.....
  thresh = 2.  ; threshold for finding stars in data
  
  pix_err = 2.0 ; pixel error between theoretical and observed position of star
  
  ;phpadu = 14.5/0.9 ; (approximate) gain (adu to electron conversion) - from brewscher et al 2010, p13
  
  ;apr = 2.0 ;[2.0,2.5,3.0] ; aperture radii (pixels)
  
  ;skyrad = [4.0,7.0]  ; sky aperture inner/outer annulus (pixels)
  
  FOR i=0, dsz[2]-1 DO BEGIN                                  ; BEGIN LOOP OVER IMAGE
  
    count = count+1
    print,'Frame number',count
    
    ;convert catalogue ra and dec coords to x,y positions on CCD
    vfov=stars2fov_bert1(ra,dec,mag,name,index[i],ccd_offsetx=0.,ccd_offsety=0.,vfov2=vfov2,frad=1.,names)
    
    ; vfov are the CCD coords (x&y) and catalogue magnitude of the targers in the FOV
    ; vfov2 are the corresponding ra and dec (catalogue)
    ; names are the target cat names 
    
    IF (vfov[0] NE -999) THEN BEGIN                           ; IF TARGET STARS ARE WITHIN FOV
   
      sz=float(n_elements(vfov))/3. ; number of target_list stars in this FOV
      
      ;find ALL peaks/stars in image above the minimum threshold
      pks=hi_findpeaks_bert1(data[*,*,i],minthresh = thresh)
      
      uniq_pks = find_uniq_objects(pks,ind = pks_ind)
      
      ;match stars to peaks
      idx = matchstars(uniq_pks,vfov) ; give the closest match - but could still be very far away1!
      
      imstars = uniq_pks[*,idx]
      
      ;if theoretical and observed star location too different ignore stars
      ii = where(abs(imstars[0,*]-vfov[0,*]) LT pix_err AND abs(imstars[1,*]-vfov[1,*] LT pix_err), nii)
      
      ; look at pks, imstars and imstars[ii,*]
      imdata=data[*,*,count]
      plot_image, bytscl(imdata[0:40,840:880], 0, 20)
      oplot, [imstars[0,ii]], [imstars[1,ii]-840.], color=cgcolor('orange'), psym=2
      
      stop
      
        
      IF (ii[0] NE -1) THEN BEGIN                             ; IF TARGET STARS MATCH PEAKS
        
    
        
        stop
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
        
        ; use vfov to make raster cutouts - quite large and save these with header info
        stereo_make_cutout, outdir, yymm, index, data[*,*,count], names, vfov, vfov2, imstars
        
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
