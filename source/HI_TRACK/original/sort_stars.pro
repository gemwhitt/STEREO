;+
;
; Name: sort_stars
;
; Purpose: If tracking multiple stars, builds arrays to account for same stars
; being observed multiple time, new stars entering fov, stars leaving
; fov etc.
;
; Syntax: sort_stars,star_mag,star_radec,star_xy,star_int,star_err,bkgd,bkgd_err,vfov,vfov2,imstars,mag,errap,sky,skyerr
;
; Inputs: star_mag - magnitude of tracked stars
;         star_radec - RA and Dec of tracked stars
;         star_xy - (x,y) position of tracked stars
;         star_int - intensity of tracked stars
;         star_err - error on the intensity of the tracked stars
;         bkgd - background intensity of tracked stars
;         bkgd_err - error on the background intensity of tracked
;                    stars
;         vfov - theoretical (x,y,mag) of stars in fov
;         vfov2 - RA and Dec of stars in fov
;         imstars - accurate (x,y) position of stars in fov
;         mag - intensity of stars in fov
;         errap - error on intensity of stars in fov
;         sky - background intensity of stars in fov
;         skyerr - error on background intensity of stars in fov
;
; Information from vfov, vfov2,imstars,mag,errap,sky and skyerr is
; added to the following arrays; star_mag,star_radec,star_xy,star_int,
; star_err,bkgd,bkgd_err
;
; History: 12-Sep-2007, Danielle Bewsher
;          24-Oct-2007, Added documentation, DB  
;
; Contact: Danielle Bewsher (d.bewsher@rl.ac.uk)
;
;-
;
PRO sort_stars,star_mag,star_radec,star_xy,star_int,star_err,bkgd,bkgd_err,vfov,vfov2,imstars,mag,errap,sky,skyerr,count

ssz = size(star_radec,/dim)
sz = size(star_mag,/dim)
IF (sz eq 1) THEN BEGIN
  nmags = 1
ENDIF ELSE BEGIN
  ;sz = size(star_mag,/dim)
  nmags = sz[0]
ENDELSE

;create temporary arrays to collect information from tracked stars
isz = size(mag,/dim)
tmp_int = fltarr(isz[0],ssz[1])*0.
tmp_err = fltarr(isz[0],ssz[1])*0.
tmp_bkgd = fltarr(ssz[1])*0.
tmp_bkgderr = fltarr(ssz[1])*0.
tmp_xy = fltarr(2,ssz[1])*0.-999.



;look for the same star in the nth frame as occurred in the n-1th frame 
;modified - vs : 24 jun 10
;This find the indices of those objects which are present in new using the old set of stars
match = match_2d(reform(star_radec[0,*]),reform(star_radec[1,*]),reform(vfov2[0,*]),reform(vfov2[1,*]),0.01)
;; The complement gets those objects which are not in the current frame but was present in previous one
chk = where(match ne -1,complement=left,nchk)
IF nchk gt 1 THEN BEGIN
      tmp_int[*,chk] = mag[*,match[chk]]
      tmp_err[*,chk] = errap[*,match[chk]]
      tmp_bkgd[chk] = sky[match[chk]]
      tmp_bkgderr[chk] = skyerr[match[chk]]
      tmp_xy[*,chk] = imstars[0:1,match[chk]]
;      
;      ;put -9's in vfov2 where a star has been checked      
      vfov2[*,match[chk]] = -9.      
ENDIF
;      ;star has left field of view
;      ;print,'Star has left fov'
      tmp_int[*,left] = 0.

IF (size(star_radec))[0] eq 2 THEN counter=1 else counter = ssz[2]
FOR i=0.,counter-1 DO BEGIN
  ii = where(star_radec[0,i] eq vfov2[0,*] and star_radec[1,i] eq vfov2[1,*],nii)

  IF (nii eq 1) THEN BEGIN
    IF (nii gt 1) THEN BEGIN
      print,'More than one star at a particular location!'
      stop
    ENDIF ELSE BEGIN
      ;print,'Tracking star'
      tmp_int[*,i] = mag[*,ii]
      tmp_err[*,i] = errap[*,ii]
      tmp_bkgd[i] = sky[ii]
      tmp_bkgderr[i] = skyerr[ii]
      tmp_xy[*,i] = imstars[0:1,ii]
	
      ;put -9's in vfov2 where a star has been checked
      vfov2[*,ii] = -9.
	
    ENDELSE
	
  ENDIF ELSE BEGIN
    ;star has left field of view
    ;print,'Star has left fov'
    tmp_int[*,i] = 0.
  ENDELSE 

ENDFOR

;concatenate star information
star_int = [[[star_int]],[[tmp_int]]]
star_err = [[[star_err]],[[tmp_err]]]

star_xy = [[[star_xy]],[[tmp_xy]]]

bkgd = [[bkgd],[tmp_bkgd]]
bkgd_err = [[bkgd_err],[tmp_bkgderr]]

ssz = size(star_int,/dim)

;look for new stars that have come into the field of view. 
;add these as new stars
new= where(vfov2[0,*] ne -9. ,nii)

IF nii gt 0 THEN BEGIN
  print,'Number of new stars in fov',nii
  ;add the new star radec and rmag to the arrays
  star_radec = [[star_radec],[vfov2[*,new]]]


  IF (nmags eq 1) THEN BEGIN
    star_mag = [star_mag,reform(vfov[2,new],nii)]
  ENDIF ELSE BEGIN
    star_mag = [[star_mag],reform(vfov[2:*,new])]
  ENDELSE

  tmp_xy = fltarr(2,nii,ssz[2])*0.-999.
  tmp_xy[*,*,ssz[2]-1] = imstars[0:1,new]


  tmp_int = fltarr(isz[0],nii,ssz[2])*0.
  tmp_int[*,*,ssz[2]-1] = mag[*,new]

  tmp_err = fltarr(isz(0),nii,ssz[2])*0.
  tmp_err[*,*,ssz[2]-1] = errap[*,new]

  tmp_bkgd = fltarr(nii,ssz[2])*0.
  tmp_bkgd[*,ssz[2]-1] = sky[new]

  tmp_bkgderr = fltarr(nii,ssz[2])*0.
  tmp_bkgderr[*,ssz[2]-1] = skyerr[new]
  
  ;concatenate new star information to data arrays 
  star_int = [[star_int],[tmp_int]]
  star_err = [[star_err],[tmp_err]]

  star_xy = [[star_xy],[tmp_xy]]
 
  bkgd = [bkgd,tmp_bkgd]
  bkgd_err = [bkgd_err,tmp_bkgderr]

ENDIF

END
