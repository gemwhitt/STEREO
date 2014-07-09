PRO ps_on, file_name, font_size, ysize=ysize, yoffset=yoffset, color=color, cc=cc, $
  landscape=landscape, invert=invert, noask=noask, xsize=xsize, xoffset=xoffset
;** Set up device for ploting postscript or X-windows with prompt;
;** author: Ivan Baldry
;** xsize standard is 17.8cm = 7.0inches
;** ysize standard is 12.7cm = 5.0inches
;** xoffset standard is 1.9cm = 0.75inches = (21.6-xsize)/2.
;** font size default is slightly larger than standard font

if n_elements(color) EQ 0 then color = 0

;** ask for type of output unless noask keyword is set
;;if keyword_set(noask) EQ 0 then begin
;;  print, string('% PS_ON: output plot as postscript to ',file_name,' ?')
;;  prompt_str = 'n'  &  read, prompt_str, prompt='% PS_ON: (n/y/e/l/p): '
;;endif else prompt_str = noask

prompt_str = 'y'

ps_yes = (prompt_str EQ 'y') or (prompt_str EQ 'e') or $
         (prompt_str EQ 'l') or (prompt_str EQ 'p')

if ps_yes then begin

  if n_elements(file_name) EQ 0 then file_name = 'temp.ps'
  if n_elements(font_size) EQ 0 then font_size = 14

  if prompt_str EQ 'e' then encapsulated=1 else encapsulated=0

;** landscape keyword sets default unless overridden by 'l' or 'p' prompts
  if prompt_str EQ 'l' then landscape=1
  if prompt_str EQ 'p' then landscape=0

  if keyword_set(landscape) EQ 0 then begin
;** set portrait mode, yoffset is for middle of US letter
    landscape=0  &  portrait=1
    if n_elements(xsize) EQ 0   then xsize = 17.0
    if n_elements(ysize) EQ 0   then ysize = 17.0
    if n_elements(xoffset) EQ 0 then xoffset = (21.6-xsize)/2.
    if n_elements(yoffset) EQ 0 then yoffset = (28.0-ysize)/2.
  endif else begin
;** set landscape mode, reverse x and y sizes wrt. offsets
    landscape=1  &  portrait=0
    if n_elements(xsize) EQ 0   then xsize = 25.0
    if n_elements(ysize) EQ 0   then ysize = 17.8
    if n_elements(xoffset) EQ 0 then xoffset = (21.6-ysize)/2.
    if n_elements(yoffset) EQ 0 then yoffset = (28.0+xsize)/2.
  endelse

;** set plot to postscipt and call device
  set_plot, 'ps'
 !p.font=0
  device, landscape=landscape, portrait=portrait, encapsulated=encapsulated, $
    file=file_name, font_size=15, $
    xsize=xsize, xoffset=xoffset, ysize=ysize, yoffset=yoffset, /color, decomposed=1, $
   /tt_font, set_font='Helvetica'
  ;;print,'% PS_ON: begin output postscript'
endif else begin 
  set_plot,'x'
endelse

;** set up color codes otherwise arbitrary grey scale
if keyword_set(color) EQ 1 then standard_colors, color=color, cc=cc, invert=invert else $
  cc=[0,0,128,128,128,0,0,128,128,128,0,0,0,255,255,255,128,128,indgen(40)*6]
  
  ;if keyword_set(color) EQ 1 then color=fsc_color()

end
