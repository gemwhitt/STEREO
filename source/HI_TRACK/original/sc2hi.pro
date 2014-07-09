;+
;
; Project: STEREO-SECCHI
;
; Name: sc2hi
;
; Purpose: To transform the given Cartesian positions in the
;          spacecraft frame of reference to the HI frame of 
;          reference. Note, this is low-level code and would
;          not usually be called directly
;
; Category: STEREO, SECCHI, HI, Calibration
;
; Explanation: For the transformation we use 4x4 transformation
;         matrices discussed in e.g., '3D Computer Graphics' by 
;         Alan Watt
;
; Syntax: out = sc2hi(vec,roll_hi_deg,pitch_hi_deg,yaw_hi_deg)
;
; Example: 
;
; Inputs: vec - (3xN) array of vector positions to transform
;         roll_hi_deg - HI roll angle relative to spacecraft (in degrees)
;         pitch_hi_deg - HI pitch angle relative to spacecraft (in
;                        degrees)
;         yaw_hi_deg - HI yaw angle relative to spacecraft (in degrees)
;
; Opt. Inputs: None
;
; Outputs: out - (3xN) array of transformed vector positions
;
; Opt. Outputs: None
;
; Keywords: None
;
; Calls: None
;
; Common: None
;
; Restrictions: None 
;
; Side effects: None
;
; Prev. Hist.: None
;
; History: Version 1, Feb-2007, Daniel Brown, Aberystwyth University 
;
; Contact: Daniel Brown (dob@aber.ac.uk)
;
;-
function sc2hi,vec,roll_hi_deg, pitch_hi_deg, offset_hi_deg


sz=size(vec)
npts=sz[2]

; pitch has different effect
theta=float(90-pitch_hi_deg)*!dpi/180.
phi=float(offset_hi_deg)*!dpi/180.
roll=float(roll_hi_deg)*!dpi/180.

; normal direction (los direction)
normz=sin(theta)*cos(phi)
normx=sin(theta)*sin(phi)
normy=cos(theta)

; an 'up' direction ('screen y')
; basic up - changed due to change of coordinate system from cart2sc
vdx=0.
vdy=1.
vdz=0.

; calculate vd.norm
vd_norm=vdx*normx + vdy*normy + vdz*normz

; calculate perpendicular up
vxtmp=vdx - vd_norm*normx
vytmp=vdy - vd_norm*normy
vztmp=vdz - vd_norm*normz

; normalise
ndiv=sqrt(vxtmp^2 + vytmp^2 + vztmp^2)
vx=vxtmp/ndiv
vy=vytmp/ndiv
vz=vztmp/ndiv

; a sideways direction ('screen x'), defined negative for left-hand rule
ux=-(normy*vz - normz*vy)
uy=-(normz*vx - normx*vz)
uz=-(normx*vy - normy*vx)

; location of eye
cx=0.
cy=0.
cz=0.

; transformation matrices for screen space
tmat=[[1., 0., 0., -cx], [0., 1., 0., -cy], [0., 0., 1., -cz], [0., 0., 0., 1.]]
rmat=[[ux, uy, uz, 0.], [vx, vy, vz, 0.], [normx, normy, normz, 0.], [0., 0., 0., 1.]]

; transformation for roll
    rollmat=[[cos(roll), -sin(roll), 0., 0.], [sin(roll), cos(roll), 0., 0.], [0., 0., 1., 0.], [0., 0., 0., 1.]]

; combine transformation matrices
    tview=rollmat##(rmat##tmat)

; apply transformation to data
    vout=fltarr(4,npts)*0.
for i=0l,npts-1 do begin
    vout(*,i)=transpose(tview##transpose(vec(*,i)))
endfor

return,vout
end
