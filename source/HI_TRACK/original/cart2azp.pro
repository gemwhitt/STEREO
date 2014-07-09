;+
;
; Project: STEREO-SECCHI
;
; Name: cart2azp
;
; Purpose: To convert points in a Cartesian frame to 2D positions seen
;          with an AZP projection with parameter mu. Note, this is a 
;          low level code, and would not usually be called directly.
;
; Category: STEREO, SECCHI, HI, Calibration
;
; Explanation: 
;
; Syntax: out = cart2azp(vec,mu)
;
; Example: 
;
; Inputs: vec - (3xN) array of vector positions to transform
;         mu - HI distortion parameter
;
; Opt. Inputs: None
;
 ; Outputs: out - 2xN array of transformed vector positions
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
function cart2azp,vec,mu

nstars=n_elements(vec[0,*])

; calculate distances from the center of the azp sphere
rr=sqrt(vec[0,*]^2 + vec[1,*]^2 + vec[2,*]^2)
rx=sqrt(vec[0,*]^2 + vec[1,*]^2)/rr
rz=vec[2,*]/rr

; calculate the planar radius in AZP
rphi=(1.0+mu)*rx/(rz+mu)

; convert this back into (x,y) coordinates
mult=rphi/rx
vout=vec
for i=0l,nstars-1 do begin
    vout[0:1,i]=mult[i]*vout[0:1,i]/rr[i]
    vout[2,i]=vout[2,i]/abs(vout[2,i])
endfor

return,vout
end
