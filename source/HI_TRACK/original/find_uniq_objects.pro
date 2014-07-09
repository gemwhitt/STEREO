Function find_uniq_objects,input,ind=keep
;+
;
; Name: resolution
;
; Purpose: To remove stars which are nearby with a magnitude criteria
;
; Syntax: 
;
; Example: output = find_uniq_objects(input,ind = keep)
;
; Inputs: input = A 2d array to find the unique position values
; Outputs : Keep = contains the indices of the uniq objects from the original
;           output = array of uniq values
;
; Calls: ord
;
; Restrictions: 
;
; History: 06 May 09 - VS based on Jeremy Balin's comment in
;http://groups.google.com/group/comp.lang.idl-pvwave/browse_thread/
;thread/641a60500c2badb7/7ccc78da05d57b64?hl=en&lnk=raot#7ccc78da05d57b64
;;
; Contact: Vino Sangaralingam -vs@star.sr.bham.ac.uk


col1ord = ulong(ord(input[0,*]))
col2ord = ulong(ord(input[1,*]))

index = col1ord + (max(col1ord)+1)*col2ord

undefine, col1ord
undefine, col2ord
;stop
h = histogram(index, reverse_indices=ri)

index=0
keep = ri[ri[where(h gt 0)]]
keep = keep[sort(keep)]

output=input[*,keep] ;contains the uniq values
return,output

END
