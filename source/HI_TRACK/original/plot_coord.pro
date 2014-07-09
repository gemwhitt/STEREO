pro plot_coord, index1, vfov2

;restore, 'data/b/20070420_20070422_L2B.sav'

index=index1

;kk=0
wcs=fitshead2wcs(index[0], system='A')

coord=wcs_get_coord(wcs)	;[2,*,*]

x1=min(coord[0,*,*])
i1=where(coord[0,*,*] eq x1)
i11=array_indices(coord[0,*,*], i1)
y1=coord[1,i11[1],i11[2]]

y2=min(coord[1,*,*])
i2=where(coord[1,*,*] eq y2)
i22=array_indices(coord[1,*,*], i2)
x2=coord[0,i22[1],i22[2]]

x3=max(coord[0,*,*])
i3=where(coord[0,*,*] eq x3)
i33=array_indices(coord[0,*,*], i3)
y3=coord[1,i33[1],i33[2]]

y4=max(coord[1,*,*])
i4=where(coord[1,*,*] eq y4)
i44=array_indices(coord[1,*,*], i4)
x4=coord[0,i44[1],i44[2]]

x=[x1,x2,x3,x4,x1]
y=[y1,y2,y3,y4,y1]

erase
ps_on, 'figs/err_fov_b.ps', xsize=15, ysize=15, noask=y
plot, vfov2[0,*], vfov2[1,*], color=fsc_color('white'), /nodata
plot, vfov2[0,*], vfov2[1,*], psym=1, xtitle='ra', ytitle='dec', /noerase
;window, 1
oplot, x+360.0, y, color=fsc_color('red')
ps_off

;window, 1, xsize=400, ysize=400
;plot, x+360.0, y


stop
end


