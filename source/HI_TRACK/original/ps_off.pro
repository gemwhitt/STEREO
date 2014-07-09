PRO ps_off
;** End output of postscript if set.
;** author: Ivan Baldry

if !d.name EQ 'PS' then begin 
  device, /close
  ;;print, '% PS_OFF: end output postscript'
  set_plot,'x'
endif

end
