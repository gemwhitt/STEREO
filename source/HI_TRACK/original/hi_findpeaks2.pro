;+
;
; Project: STEREO-SECCHI
;
; Name: hi_findpeaks2
;
; Purpose: Identifies peaks above a threshold in an image. The code
;          looks for local maximum value pixels in subsquares (of 
;          user defined size) 
;
; Category: STEREO, SECCHI, HI, Calibration
;
; Explanation: 
;
; Syntax: pks = hi_findpeaks2(data,minthresh=mthr,spread=spr)
;
; Example: 
;
; Inputs: data - one image
;
; Opt. Inputs: minthresh - threshold above which peaks are to be found
;              spread - radius of square over which to find individual peaks
;
; Outputs: pks - array of (x,y) positions of peaks identified
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
; Prev. Hist.: hi_findpeaks.pro initally written Feb-2007 
;
; History: June 2007, Daniel Brown, Aberystwyth University 
;
; Contact: Daniel Brown (dob@aber.ac.uk)
;
;-
FUNCTION hi_findpeaks2, data, minthresh=mthr, spread=spr


; mthr says how much larger the intensity must be over the background
; to be included as a peak
; spr defines the size of a small square in which to search for
; individual peaks. This is like an averaging parameter and says that
; if two peaks are this close together then they are just noise in a
; single peak

IF (n_elements(mthr) EQ 0) THEN mthr=10000
IF (n_elements(spr) EQ 0) THEN spr=2

sz=size(data)

; store all our valid peaks in this variable
pks=[-999.9,-999.9,-999.9]

;histoplot,data,binsize=0.1,maxinput=2.0,/line_fill,orientation=[45,-45],title='In Hi find peaks'
; mp gives the linear location of the centre of the cutout square
mp=((2*spr + 1)^2 + 1)/2-1

; loop through our data
FOR j=spr,(sz[2]-spr-1) DO BEGIN


    FOR i=spr, (sz[1]-spr-1) DO BEGIN


        ; an initial test for validity of data, cuts down on messing
        ; about with poxy data

	     IF (data[i,j] GE mthr) THEN BEGIN
            ; cutout a small square for peak searching
            cutout=data(i-spr:i+spr,j-spr:j+spr)


             ; calculate an average background of the cutout
            bb=sort(cutout)
            avb=median(cutout[bb[0:2*spr]])

            ; find location of highest peak in cutout
            aa=where(cutout EQ max(cutout))
	
            ; a peak is registered if it is at the centre of the cutout
            ; and it is significantly greater in intensity than the
            ; local background 
	
            IF (aa[0] EQ  mp) AND (data[i,j]-avb GT mthr) THEN BEGIN
	
		
		            x = 0.
                fx = 0.
                y = 0.
                fy = 0.
                FOR l = -spr,spr DO BEGIN
                  FOR m=-spr,spr DO BEGIN
                    x = x + float(i+m)*float(data[i+m,j+l])
                    fx = fx + float(data[i+m,j+l])
                    y = y + float(j+l)*float(data[i+m,j+l])
                    fy = fy + float(data[i+m,j+l])
                  ENDFOR
                ENDFOR

	              ;x = float(i-1)*float(data(i-1,j))+float(i)*float(data(i,j))+float(i+1)*float(data(i+1,j))
                ;fx = float(total(data(i-1:i+1,j)))
                ;y = float(j-1)*float(data(i,j-1))+float(j)*float(data(i,j))+float(j+1)*float(data(i,j+1))
                ;fy = float(total(data(i,j-1:j+1)))
                pks=[[pks],[x/fx,y/fy,data[i,j]]]
            ENDIF
           ENDIF
    ENDFOR

ENDFOR





; remove the dummy first value ([-999, -999, -999], although this is
; returned if no other peaks are found.

npks=n_elements(pks[0,*])

if (npks gt 1) then begin
pks=pks(*,1:npks-1)

ENDIF

return,pks
END
