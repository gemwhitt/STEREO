;+
;
; Name: track_stars
;
; Purpose: Tracks stars in a series of HI images 
;
; Syntax: track_stars,index,data,ra,dec,mag,outfile
;
; Example: 
;
; Inputs: index - array of index structures
;         data - data cube to search for stars
;         ra - array of RA's of stars to track
;         dec - array of Dec's of stars to track
;         mag - array of magnitudes of stars to track (can be set to
;               zero if you're not bothered about magnitudes - but
;               must be entered)
;         outfile - path to save file to save the output
;
; Calls: stars2fov, hi_findpeaks2, matchstars, aper, sort_stars, steve_centroid, resolution
;
; Restrictions: File list must be from the same spacecraft and same
;               camera - cannot mix and match data from different HI cameras
;
; History: Version 1, 24-Oct-2007, Danielle Bewsher 
;                      7-Nov-2007, Steve Spreckley - corrected a number of
;                                                    errors in the code.
;                      8-Nov-2007, SAS - enabled multiple data blocks
;                                  to be passed consecutively to avoid
;                                  filling memory
;                     12-Nov-2007, SAS - fixed problem from using mag as
;                                  a variable for both star catalogue magnitude and
;                                  measured flux
;                                  mag = star catalogue magnitude
;                                  flux = measured flux
;     18-Feb-2008,Vino- Added fixed offsets to CCD centres, added a magnitude-distance criterion in stars tracked
;     01-May-2008,Vino- Modified distance criterion using Tycho catalogue 
;     10-Aug-2008, Vino - Made corrections to the CCD offsets supplied after interation 
;      10-Feb-2009, Vino - Removed the CCD corrections due to updated pointing information
;                           fixed errors with centroiding by setting all NaN values to 0.
;                           fixed nparams as n_params()
;      15 Aug 2010, Vino - Made the flux values summed rather than averaged, changed the sky radius values 
;                         according to calibration paper
;      09 sep 2010, Vino - modified to get the output directory from the input file itself 
; Contact: Danielle Bewsher (d.bewsher@rl.ac.uk)
;
;-
PRO track_stars3b,data_files,catalogue_files,outfile, display=display

          devicelib
          imagelib
          

Compile_Opt idl2 ; this makes all the values intrinsically long 

IF n_params() lt 2 THEN BEGIN
   printf, 'please enter an (array of) data files, ras, decs, and mags'
  return
END

  ;set threshold for finding peaks/stars in data
 thresh = 1.5

;set up parameters for aperture photometry
;(approximate) gain (adu to electron conversion)

phpadu = 14.5/0.9 ;old value
;phpadu = 15.0/0.9 ; from brewscher et al 2010, p13
;aperture radii (pixels)

apr = [2.5,3.0,3.5] ;old value

;apr = [3.2];brewscher test value
;sky aperture inner/outer annulus (pixels)
skyrad = [6.0,10.0];old value
;skyrad = [4.7,7.9] ;- brewscher plot analysis value
;skyrad = [6.0, 9.0]

;set up counter to count frame numbers
count = -1
catch_stars = -1
j=-1 & no=00

;pixel error between theoretical and observed position of star
pix_err = 3.; changed from 3 in previous analysis;1.5

IF keyword_set(display) THEN BEGIN
 ; set up window to display image
  window,0,xsize=500,ysize=500

ENDIF

ndata_files = n_elements(data_files)
print,'No of data files = ',ndata_files

FOR kk = 0, ndata_files-1 DO BEGIN
   ;restore, data_files[kk]  ; restore a .sav file containing the cleaned images - SAS
  
   restore,data_files[kk] ; these are downloaded L2 data files
   index = index1; I have chosen to call the header structure index1 NOT index - SAS
   
   inst = 0

   dsz = size(data)
   data = data*4. ; to make sure that the flux is summed rather averaged
  
   ;run through images one by one
   FOR i=0,dsz[3]-1 DO BEGIN
  
      count = count+1
      print,'Frame number',count,'no=',no,'j=',j
      
      restore,file_search(catalogue_files)  ;file contains the variables DEC, MAG, RA

      print, 'Total Stars in Catalogue : '+strcompress(string(n_elements(ra)), /rem)
      
       ;convert star positions to x,y positions in image        --  corrected index to index[i]
    
      vfov = stars2fov(ra,dec,mag,index[i],ccd_offsetx=0.,ccd_offsety=0.,vfov2=vfov2,frad=0.2)
      
      undefine, ra, dec, mag
      
     IF (vfov[0] NE -999) THEN BEGIN
     
     ;Find only the uniq values of both the peaks and the catalogue position - vs 06 11 09
      ;vfov = find_uniq_objects(vfov,ind=obj_ind)
      ;vfov2 = vfov2[*,obj_ind]
     
      vfov1 = vfov
    
;      print,'Resolving crowded issue'
      rsl = resolution(vfov1) ; ignores stars which are close to each other set by a magnitude criterion - Vino,revised 02/09/10

      vfov2=vfov2[*,rsl]
      vfov=vfov[*,rsl]

        ;find peaks/stars in image

        pks =hi_findpeaks2(data[*,*,i],minthresh = thresh)

        uniq_pks = find_uniq_objects(pks,ind = pks_ind)
      
        ;match stars to peaks
        ;idx = matchstars(pks,vfov)

        idx = matchstars(uniq_pks,vfov)

        ;imstars = pks(*,idx)

        imstars = uniq_pks[*,idx]

        ;if theoretical and observed star location too different ignore stars
         ii = where(abs(imstars[0,*]-vfov[0,*]) LT pix_err AND abs(imstars[1,*]-vfov[1,*] LT pix_err))
  
        IF (ii[0] NE -1) THEN BEGIN
                
          imstars = imstars[*,ii]
          vfov = vfov[*,ii]
          vfov2 = vfov2[*,ii]
          
          catch_stars = catch_stars+1
         j=j+1
        
    
          imstars2 = steve_centroid(data[*,*,i],imstars,fwhm=2.0) ;  actual values found from the HI paper -1.6
                   
          isz = size(imstars2)

          IF keyword_set(display) THEN BEGIN

            ;plot image with star positions marked
            plot_image,data[*,*,i],min=0,max=2,title=index[i].date_d$obs,color=fsc_COLOR('WHITE'), /nosquare       
            plots,imstars2[0,*],imstars2[1,*],psym=4,color=fsc_color('red')
            ;xyouts,50,6,i,charsize=1.5
            
            
          ENDIF
          ;do aperture photometry on the stars
          aper,data[*,*,i],imstars2[0,*],imstars2[1,*],flux,errap,sky,skyerr,phpadu,apr,/silent,skyrad,/nan,$
              /flux,/exact

          ;work out size of arrays etc so we know how to save everything
          ndim = size(imstars2,/n_dimensions)
          IF (ndim EQ 1) THEN BEGIN
            isz = 1
          ENDIF ELSE BEGIN
           isz = (size(imstars2))[2]
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
  
          ENDIF ELSE BEGIN
           ;if not first frame, then build up arrays
           sort_stars,star_mag,star_radec,star_xy,star_int,star_err,bkgd,bkgd_err,vfov,vfov2,imstars2,flux,errap,sky,skyerr,count
           tim = [tim,index[i].date_d$obs];has to be d$obs for normal analysis
           sun_time = [sun_time,index[i].sun_time]
          ENDELSE
    
     
        ENDIF 
     ENDIF ELSE BEGIN
       print,'No stars found'
     ENDELSE
   
  ;save the results intermediately and initialise the counters (Vino 19/06/08)
 
 
  IF (j EQ 500) THEN BEGIN

   IF no lt 10 THEN nos='0'+strtrim(string(no),2) ELSE nos = no
   IF not keyword_set(outfile) THEN $
   outpath = file_dirname(data_files[kk])+'_'+strtrim(string(nos),2)+'.sav' $
   ELSE $
   outpath=outfile+'_'+strcompress(string(nos),/rem)+'.sav' 
   
   tim_jd = anytim2jd(tim)
   tim_jd = (tim_jd.int+tim_jd.frac)-sun_time
   save,filename=outpath,star_mag,star_radec,sun_time,star_xy,star_int,star_err,bkgd,bkgd_err,tim,tim_jd
   print,'Saved file:',outpath
   print,'Last frame date :',tim[500]
   no=no+1
   j=-1
   catch_stars=-1
   
  ENDIF
 

ENDFOR
ENDFOR
   

   ;save final output
  
  
   tim_jd = anytim2jd(tim)
   tim_jd = [tim_jd.int+tim_jd.frac]-sun_time
   
    IF no lt 10 THEN nos='0'+strtrim(string(no),2) ELSE nos = no
   IF not keyword_set(outfile) THEN $
   outpath = file_dirname(data_files[kk])+'_'+strtrim(string(nos),2)+'.sav' $
   ELSE $
   outpath=outfile+'_'+strtrim(string(nos),2)+'.sav'
   
     
     save,filename=outpath,star_mag,star_radec,sun_time,star_xy,star_int,star_err,bkgd,bkgd_err,tim,tim_jd
   
  print,'Last Saved file:',outpath 

END
