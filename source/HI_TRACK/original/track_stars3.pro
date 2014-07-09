;+
;
; Name: track_stars3
;
; Purpose: Tracks stars in a series of HI images 
;
; Syntax: track_stars,index,data,ra,dec,mag,outfile
;;
; Inputs: index - array of index structures
;         data - data cube to search for stars
;         ra - array of RA's of stars to track
;         dec - array of Dec's of stars to track
;         mag - array of magnitudes of stars to track (can be set to
;               zero if you're not bothered about magnitudes - but
;               must be entered)
;         outfile - path to save file to save the output
;
; Calls: stars2fov, resolution2, hi_findpeaks2, find_uniq_objects, matchstars, 
;        steve_centroid, plot_image, aper, sort_stars, anytim2jd
;
; Restrictions: File list must be from the same spacecraft and same
;               camera - cannot mix and match data from different HI cameras
;
; History: 
; Version 1, 24-Oct-2007, Danielle Bewsher 
;            7-Nov-2007, Steve Spreckley - corrected a number of errors in the code.
;            8-Nov-2007, SAS - enabled multiple data blocks to be passed consecutively to 
;                        avoid filling memory.
;            12-Nov-2007, SAS - fixed problem from using mag as a variable for both star catalogue 
;                         magnitude and measured flux:
;                         mag = star catalogue magnitude
;                         flux = measured flux
;            18-Feb-2008, Vino- Added fixed offsets to CCD centres, added a magnitude-distance 
;                         criterion in stars tracked.
;            01-May-2008,Vino- Modified distance criterion using Tycho catalogue 
;            10-Aug-2008, Vino - Made corrections to the CCD offsets supplied after interation 
;            10-Feb-2009, Vino - Removed the CCD corrections due to updated pointing information
;                         fixed errors with centroiding by setting all NaN values to 0.
;                         fixed nparams as n_params().
;            07-May-2010, Vino - Modified tim_jd to include sun_time.
;            15 Aug 2010, Vino - Made the flux values summed rather than averaged, changed the sky 
;                         radius values according to calibration paper.
;            09 Sep 2010, Vino - modified to get the output directory from the input file itself 
; 
; Contact: Danielle Bewsher (d.bewsher@rl.ac.uk)
;
;-
PRO track_stars3, dir, data_files,catalogue_files,outfile, opt, display=display

; opt is either 'a' or 'b'

          devicelib   ; adds system variables !BCOLOR and !ASPECT needed for plotting
          imagelib    ; adds system variable !IMAGE needed for plotting

Compile_Opt idl2 ; this makes all the values intrinsically long 

IF n_params() lt 2 THEN BEGIN
   printf, 'please enter an (array of) data files, ras, decs, and mags'
  return
END

  ;set threshold for finding peaks/stars in data
 thresh = 4.

;set up parameters for aperture photometry
;(approximate) gain (adu to electron conversion)

phpadu = 14.5/0.9 ;old value
;phpadu = 15.0/0.9 ; from brewscher et al 2010, p13

;aperture radii (pixels)
apr = [2.5,3.0,3.5] 

;sky aperture inner/outer annulus (pixels)
skyrad = [6.0,10.0];old value
;skyrad = [6.0, 9.0]

;;;set up counter to count frame numbers - MOVE THIS TO INSIDE THE MAIN LOOP (29/07 - TEST)
;;count = -1
;;catch_stars = -1
;;j=-1 & no=00

;pixel error between theoretical and observed position of star
pix_err = 1.0; changed from 3 in previous analysis;1.5

IF keyword_set(display) THEN BEGIN
 ; set up window to display image
  window,0,xsize=500,ysize=500

ENDIF

ndata_files = n_elements(data_files)

print,'No of data files = ',ndata_files

FOR kk = 2, ndata_files-1 DO BEGIN ; split up between two jobs else -- ndata_files-1 DO BEGIN ; original
;FOR kk = 4, 4 DO BEGIN  ;for tracking tests 

print, data_files[kk]

;;;set up counter to count frame numbers
count = -1
catch_stars = -1
j=-1 & no=00

   restore,data_files[kk] ; these are downloaded L2 data files

   index = index1 ; I have chosen to call the header structure index1 NOT index - SAS
   inst = 0

   dsz = size(data)
   data = data*4. ; to make sure that the flux is summed rather averaged

   ;run through images one by one
   FOR i=0, dsz[3]-1 DO BEGIN
  
      count = count+1
      print,'Frame number',count,'no=',no,'j=',j
      
      restore,file_search(catalogue_files)  ;file contains the variables DEC, MAG, RA

       ;convert star positions to x,y positions in image  --  corrected index to index[i]
      vfov=stars2fov(ra,dec,mag,index[i],ccd_offsetx=0.,ccd_offsety=0.,vfov2=vfov2,frad=1.02)
                     
      undefine, ra, dec, mag  ; to free up space
      
     IF (vfov[0] NE -999) THEN BEGIN
     
      cat_stars_fov=(size(vfov, /dim))[1]
     
      ; for purpose of creating example image for thesis - only keep a random selection of stars
      ; first find size of vfov array
      sz=(size(vfov,/dim))[1]
      ; now pick random numbers from 0 to sz 
      
      sample=fix(randomu(seed,1000)*sz)
      
      vfov1 = vfov[*,sample]
      vfov=vfov[*,sample]
      
      sz=(size(vfov1,/dim))[1]
      
      if sz lt 35000 then begin
    
      rsl = resolution(vfov1)  ; ignores stars which are close to each other set by a magnitude criterion - Vino,revised 02/09/10
      
      vfov=vfov[*,rsl]
      vfov2=vfov2[*,rsl]
      
      cat_stars_rsl=size(rsl, /dim)
      
      endif else begin 
      
      resolution2, vfov1, vfov2
      
      vfov=vfov1
      
      cat_stars_rsl=999999
      
      endelse  
   
        ;find peaks/stars in image
        pks =hi_findpeaks2(data[*,*,i],minthresh = thresh)
        
        pks_data=(size(pks, /dim))
        if pks[0] ne -999.900 then pks_data=pks_data[1]

        uniq_pks = find_uniq_objects(pks,ind = pks_ind)
      
        ;match stars to peaks
        idx = matchstars(uniq_pks,vfov)
        
        match_stars1=size(idx, /dim)

        imstars = uniq_pks[*,idx]

        ;if theoretical and observed star location too different ignore stars
         ii = where(abs(imstars[0,*]-vfov[0,*]) LT pix_err AND abs(imstars[1,*]-vfov[1,*] LT pix_err))
  
        match_stars2=size(ii, /dim)
  
        IF (ii[0] NE -1) THEN BEGIN
                
          imstars = imstars[*,ii]
          vfov = vfov[*,ii]
          vfov2 = vfov2[*,ii]
          
          catch_stars = catch_stars+1
          j=j+1
        
          imstars2 = steve_centroid(data[*,*,i],imstars,fwhm=2.0) ;  actual values found from the HI paper -1.6
                   
          isz = size(imstars2)
          
          stars_tracked=isz[2]

          IF keyword_set(display) THEN BEGIN
            ;plot image with star positions marked
            plot_image,data[*,*,i],min=0,max=100,title=index[i].date_d$obs,/nosquare, color=fsc_COLOR('black')
            plots,imstars2[0,*],imstars2[1,*],psym=4,color=fsc_color('white')
            ;xyouts,50,6,i,charsize=1.5        
          ENDIF
          
          ;;start of outout image testing
          ;if i eq 0 then begin      
          trackfig=outfile+file_basename(data_files[kk], '.sav')+'.ps'
          ps_on, trackfig, /noask
          plot_image,data[*,*,i],min=0,max=2,title=index[i].date_d$obs,color=fsc_color('black'), /nosquare
          plots,imstars2[0,*],imstars2[1,*],psym=4,color=cgcolor('orange')
          ps_off 
          ;spawn, 'bash /bb/phy/gnw973/HI_TRACK/resource/bluebear_scp.sh '+trackfig+' '+opt+' '+'track_figs_new'
          stop
          ;spawn, 'scp '+trackfig+' gemma@ull.sr.bham.ac.uk:/data2/gemma/stereo_new/'+opt+'/track_figs/'
          ;spawn, 'rm '+trackfig
          
          openw, lun, dir+'/data/'+opt+'/track_info_pe1.txt', /get_lun, /append
          printf, lun, file_basename(data_files[kk], '.sav')
          printf, lun, cat_stars_fov, cat_stars_rsl, pks_data, match_stars1, match_stars2, stars_tracked, $
          format='(i, x, i, x, i, x, i, x, i, x, i)'
          free_lun, lun 
                   
          ;endif
         stop
          ;do aperture photometry on the stars
          aper,data[*,*,i],imstars2[0,*],imstars2[1,*],flux,errap,sky,skyerr,phpadu,apr,/silent,skyrad,/nan,$
              /flux,/exact

          ;work out size of arrays etc so we know how to save everything
          ndim = size(imstars2,/n_dimensions)
          IF (ndim EQ 1) THEN BEGIN
            isz = 1
          ENDIF ELSE BEGIN
           isz = (size(imstars2))[2]    ; isz is number of stars tracked in this image 
          ENDELSE

;;; ------------------ WE REALLY NEED TO RECODE THIS AS IT WILL ONLY HANDLE A
;;; ------------------ FEW HUNDRED STARS AND WE ARE WASTING A LOT OF MEMORY
;;; ------------------ WITH ZEROS IN THE ARRAYS WHEN STARS HAVE LEFT OR ARE 
;;; ------------------ YET TO ENTER THE IMAGE

          ;if first frame then set up arrays which we will add to
          IF (catch_stars EQ 0) THEN BEGIN
           star_mag = reform(vfov[2,*],isz)
           star_radec = reform(vfov2[0:1,*],2,isz)
           star_xy = reform(imstars2[0:1,*],2,isz)
           star_int = flux
           star_err = errap
           bkgd = sky
           bkgd_err = skyerr
           ;store date_obs
           tim = index[i].date_d$obs
           sun_time = index[i].sun_time
           ; store ravg to check pointing solutions are accurate
           ravg=index[i].ravg
;  stop
          ENDIF ELSE BEGIN
           ;if not first frame, then build up arrays
           ;sort_stars,star_mag,star_radec,star_xy,star_int,star_err,bkgd,bkgd_err,vfov,vfov2,imstars2,flux,errap,sky,skyerr,count
           print, 'sort stars'
           ;29/07/2011 - testing sort_stars
           sort_stars2,star_mag,star_radec,star_xy,star_int,star_err,bkgd,bkgd_err,vfov,vfov2,imstars2,flux,errap,sky,skyerr,count
           
           tim = [tim,index[i].date_d$obs];has to be d$obs for normal analysis
           sun_time = [sun_time,index[i].sun_time]
           ravg=[ravg, index[i].ravg]
          ENDELSE
     
        ENDIF 
     ENDIF ELSE BEGIN
       print,'No stars found'
     ENDELSE
   
  ;save the results intermediately and initialise the counters (Vino 19/06/08)
 
  ;;IF (j EQ 500) THEN BEGIN

;;   IF no lt 10 THEN nos='0'+strtrim(string(no),2) ELSE nos = no
  ;; IF not keyword_set(outfile) THEN $
  ;; outpath = file_dirname(data_files[kk])+'_'+strtrim(string(nos),2)+'.sav' $
   ;;ELSE $
   ;;;outpath=outfile+'_'+strcompress(string(nos),/rem)+'.sav' 
   ;;outpath=outfile+file_basename(data_files[kk],'_L2B.sav')+'_L3B.sav' 
   
  ;; tim_jd = anytim2jd(tim)
   ;;tim_jd = (tim_jd.int+tim_jd.frac)-sun_time
   ;;save,filename=outpath,star_mag,star_radec,sun_time,star_xy,star_int,star_err,bkgd,bkgd_err,tim,tim_jd
   ;;print,'Saved file:',outpath
   ;;print,'Last frame date :',tim[500]
   ;;no=no+1
   ;;j=-1
   ;;catch_stars=-1
   
  ;;ENDIF
  
           
  ENDFOR

   ;save final output
  
   tim_jd = anytim2jd(tim)
   tim_jd = [tim_jd.int+tim_jd.frac]-sun_time ; modified 07/05/2010 to include the sun time
   
    ;IF no lt 10 THEN nos='0'+strtrim(string(no),2) ELSE nos = no
  ;; IF not keyword_set(outfile) THEN $
  ; outpath = file_dirname(data_files[kk-1])+'_'+strtrim(string(nos),2)+'.sav' $
 ;  ELSE $
   ;outpath=outfile+'_'+strtrim(string(nos),2)+'.sav'
   
   ;OLD;;;;;;;;;;;;;;;;;;;;
   ;if opt eq 'a' then outpath=outfile+file_basename(data_files[kk],'_L2a.sav')+'_L3a.sav'  else $   ; for A data
   ;outpath=outfile+file_basename(data_files[kk],'_L2b.sav')+'_L3b.sav'       ; for B data
   
   ;NEW;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   outpath=outfile+file_basename(data_files[kk],'_L2'+opt+'.sav')+'_L3'+opt+'.sav'
    
     save,filename=outpath,star_mag,star_radec,sun_time,star_xy,star_int,star_err,bkgd,bkgd_err,tim,tim_jd,ravg
     
      ; Determine which instrument the light curves is from using the filename
     ;file=file_basename(outpath, '.sav')
     ;file_len=strlen(file)
     ;a_or_b=strmid(file, file_len-1)  ; gives 'A' or 'B'
     
     ; spawn scp to copy file back to ull and then delete file from bluebear 
     ;spawn, 'bash /bb/phy/gnw973/HI_TRACK/resource/bluebear_scp.sh '+outpath+' '+opt+' '+'l3'
          
     ;print,'Last Saved file:',outpath 
     
     ;spawn, 'rm '+data_files[kk]
     
     ;spawn, 'mv '+data_files[kk]+' /bb/phy/gnw973/nbu/'+opt+'/l2/'  ; move input (L2) file to nbu (250GB - total)

ENDFOR

END
