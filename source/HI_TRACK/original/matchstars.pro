;+
;
; Project: STEREO-SECCHI
;
; Name: matchstars
;
; Purpose: To try to match previously found peaks in the observation
;          to known positions of, e.g., stars. The star positions
;          should have already been calculated in AZP.
;
; Category: STEREO, SECCHI, HI, Calibration
;
; Explanation: 
;
; Syntax: out = matchstars(pks,vfov,ravg=ravg,rav2=ravg2,nstars=nct)
;
; Example: 
;
; Inputs: pks - first array of (x,y) positions of features in an image
;         vfov - second array of (x,y) positions of features in an image 
;
; Opt. Inputs: None
;
; Outputs: out - this is an index of positions in pks that match with
;                positions in vfov. For example, the ith star in vfov
;                (e.g. (vfov(*,i) ) has been matched with the out(i)th
;                peak in pks (e.g., pks(out(i)) )
;
; Opt. Outputs: ravg - average radial distance between features in
;                      an image
;               rav2 - average radial distance between features in
;                      an image, neglecting the outliers
;               nstars - number of features in pks and vfov that have
;                        been 'matched'
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
;          Version 2, 26-Sept-07 Danielle Bewsher (RAL) 
;                     Made loop 'long'
;
; Contact: Daniel Brown (dob@aber.ac.uk)
;
;-
FUNCTION matchstars,pks,vfov,ravg=ravg,rav2=ravg2,nstars2=nct

nst=n_elements(vfov[0,*])
idx=intarr(nst)
avg=0.0
avg2=0.0
nct=0

FOR i=0l,nst-1 DO BEGIN
    r=sqrt((pks[0,*]-vfov[0,i])^2 + (pks[1,*]-vfov[1,i])^2)
    aa=where(r EQ min(r,/NaN),count)
 
    IF count GT 0. THEN BEGIN
    idx[i]=aa[0]
    avg=avg+r(aa[0])
    ;IF r(aa[0]) gt 10. THEN idx(i)=aa(0)
    IF (r(aa[0]) LT 2.) THEN BEGIN
        
        avg2=avg2+r(aa[0])
        nct=nct+1
    ENDIF
    ENDIF
ENDFOR

;ravg=sqrt(avg/float(nst))
;ravg2=sqrt(avg2/float(nct))
return,idx

END
