;+
; NAME:
;    ORD
;
; PURPOSE:
;    Calculates the ordinal of each value of an array in terms of the
;    sorted values. This can be very useful for shrinking sparse
;    arrays before using histogram.
;
; CATEGORY:
;    Math
;
; CALLING SEQUENCE:
;    Result = ORD(Values)
;
; INPUTS:
;    Values:  A vector of values.
;
; EXAMPLE:
;    Calculate the ordinals of values in an array.
;
;    IDL> array = [5,6,7,4,5,6,-2]
;    IDL> print, ord(array)
;               2           3           4           1           2           3
;               0
;
; OUTPUTS:
;    Returns a long array with the same number of elements as Values,
;    where each value is replaced by its ordinal (starting at 0).
;    Identical values are given the same ordinal.
;
; MODIFICATION HISTORY:
;    Written by:      Jeremy Bailin
;    28 March 2009    Public release in JBIU
;-
function ord, values

nvalues=n_elements(values)
sortvalues = sort(values)
uniqvalues = uniq(values[sortvalues])

nuniq = n_elements(uniqvalues)
ordlist = lindgen(nuniq)

; this is basically the histogram(total(/cumulative)) trick
outp = lonarr(nvalues)
IF nuniq gt 1 THEN BEGIN
h = histogram(uniqvalues,bin=1,min=0,reverse=ri)
outp[sortvalues] = ordlist[ri[0:nvalues-1]-ri[0]]
ENDIF ELSE outp[sortvalues] = ordlist

return, outp

end
