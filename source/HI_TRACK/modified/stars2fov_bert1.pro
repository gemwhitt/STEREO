function stars2fov_bert1,ra,dec,mag,name,index,yaw=yaw,pitch=pitch,roll=roll,hi_offsetx=offset_hi,hi_offsety=pitch_hi,hi_roll=roll_hi,mu=mu,fov=d,ccd_offsetx=ccdosx,ccd_offsety=ccdosy,pixmult=pmult,frad=frad,indexout=aa,vfov2=vfov2,system=sys,names

; Calls: cart2sc, sc2hi, cart2azp

  if (n_elements(sys) eq 0) then sys='gei'
  
  ; Get the spacecraft pointing - two sets of header values depending on coord system
  if (sys eq 'gei') then begin
    if (n_elements(yaw) eq 0) then yaw = index.sc_yawa
    if (n_elements(pitch) eq 0) then pitch = index.sc_pita
    if (n_elements(roll) eq 0) then roll = index.sc_rolla
  endif else begin
    if (n_elements(yaw) eq 0) then yaw = index.sc_yaw
    if (n_elements(pitch) eq 0) then pitch = index.sc_pitch
    if (n_elements(roll) eq 0) then roll = -index.sc_roll
  endelse
  
  ; Get the particular HI relative pointing and other instrument parameters
  get_hi_params,index,pitch_hi,offset_hi,roll_hi,mu,d
  
  ; Set a few other bits and pieces
  if (n_elements(ccdosx) eq 0) then ccdosx=0.
  if (n_elements(ccdosy) eq 0) then ccdosy=0.
  if (n_elements(frad) eq 0) then frad=1.
  if (n_elements(pmult) eq 0) then pmult=0.5*float(index.naxis1)
  
  ; convert from RA-Dec to Cartesian coordinates, placing stars on
  ; a unit sphere. z is up, x is into the screen and y makes up the
  ; system
  ang=(90. - 0.5*d)*!dpi/180.
  rng=((1.0+mu)*cos(ang))/(sin(ang)+mu)
  
  
  xst=sin((90-dec)*!pi/180.)*cos(ra*!pi/180.)
  yst=sin((90-dec)*!pi/180.)*sin(ra*!pi/180.)
  zst=cos((90-dec)*!pi/180.)
  
  
  ; as we use 4x4 transformation matrices, we need a 4th scale parameter for each point
  nst=n_elements(xst)
  ll=fltarr(nst)*0. + 1.0
  
  vv=transpose([[xst],[yst],[zst],[ll]])
  vv = reform(vv,4,nst)
  
  ; convert from RA-Dec to spacecraft frame of reference (i.e., x-axis
  ; now points along direction that the spacecraft points
  vv2=cart2sc(vv,roll,pitch,yaw)
  vv2 = reform(vv2,4,nst)
  ; convert from spacecraft frame to HI frame
  vv3=sc2hi(vv2,roll_hi,pitch_hi,offset_hi)
  vv3 = reform(vv3,4,nst)
  ; convert from Cartesian to AZP
  vout=cart2azp(vv3,mu)
  vout = reform(vout,4,nst)
  
  ; rescale to pixel units
  vout[0:2,*]=vout[0:2,*]/rng
  
  ; determine which stars are in the HI fov and are in front of the instrument rather than behind it
  aa=where((vout[2,*] gt 0)and (vout[0,*] ge -frad) and (vout[0,*] le frad) and $
     (vout[1,*] ge -frad) and (vout[1,*] le frad), naa)

  if naa gt 0 then begin
    
    ; create output and align coordinates to match pixel coordinates of
    ; supplied header information. Add star magnitudes to output
    vfov=fltarr(3,naa)
    vfov[0:1,*]=vout[0:1,aa]
    vfov[0,*]=(vfov[0,*]+1.0)*pmult + ccdosx
    vfov[1,*]=(vfov[1,*]+1.0)*pmult + ccdosy
    vfov[2,*]=mag(aa)
    names=name(aa)
    
    ;save ra and dec of points returned in vfov
    vfov2 = vfov[0:1,*]
    vfov2[0,*] = ra[aa]
    vfov2[1,*] = dec[aa]
    
  ENDIF ELSE BEGIN
    print,'No stars in fov'
    vfov = [-999,-999,-999]
    vfov2 = [-999,-999,-999]
    names=[]
  ENDELSE
  
  return,vfov
end
