pro draw_bowtie,orbit,scene,data=data,color=color,lats=lats,lons=lons,draw_coast=draw_coast,xrange=xrange,yrange=yrange,_extra=extra
  restore,file_which('coast.sav')
  if n_elements(data) eq 0 then begin
    path='/aim/data/cips/v3.12'
    file=read_cips_file_bynum(path,'1a',orbit,'px')
    data=replicate(file[0],30,4)
    data[0:29,0]=file
    file=read_cips_file_bynum(path,'1a',orbit,'mx')
    data[3:29,1]=file
    file=read_cips_file_bynum(path,'1a',orbit,'py')
    data[3:29,2]=file
    file=read_cips_file_bynum(path,'1a',orbit,'my')
    data[3:29,3]=file
    file=0
    heap_gc
  end
  sc_lat=data[scene,0].sc_latitude
  sc_lon=data[scene,0].sc_longitude
  print,sc_lat,sc_lon
  
  lat=fltarr(170,340,4)
  lon=fltarr(170,340,4)
  for i=0,3 do begin
    lat[*,*,i]=*data[scene,i].latitude
    lon[*,*,i]=*data[scene,i].longitude
  end
  rho=gcdist(sc_lat,sc_lon,lat,lon,/deg)*!dtor*6378.137
  theta=gcaz(sc_lat,sc_lon,lat,lon,/deg)*!dtor


  logical_north=theta(0,170,3)
;  logical_north=0

  set_plot,'ps'
  device,filename='bowtie_polyfill.eps',/encapsulated,/color
  loadct,39
  plot,/nodata,psym=3,rho*sin(theta-logical_north),rho*cos(theta-logical_north), $
                    /isotropic,charsize=1.0,xtitle='Alongtrack (km)',ytitle='Crosstrack (km)', $
                    background=255,color=0,xrange=xrange,yrange=yrange,_extra=extra
  if n_elements(xrange) eq 0 then xrange=!x.crange
  if n_elements(xrange) eq 0 then yrange=!y.crange
  for i=0,3 do begin
    r1=(rho[*,0,i])[*]
    r2=(rho[169,*,i])[*]
    r3=reverse((rho[*,339,i])[*])
    r4=reverse((rho[0,*,i])[*])
    q1=(theta[*,0,i])[*]
    q2=(theta[169,*,i])[*]
    q3=reverse((theta[*,339,i])[*])
    q4=reverse((theta[0,*,i])[*])
    r=[r1,r2,r3,r4]
    q=[q1,q2,q3,q4]
    polyfill,r*sin(q-logical_north),r*cos(q-logical_north),color=color[i]
;    for k=0,339,2 do begin
;      oplot,rho[*,k,i]*sin(theta[*,k,i]-logical_north),rho[*,k,i]*cos(theta[*,k,i]-logical_north),color=color[i]
;    end
;    for j=0,169 do begin
;      oplot,rho[j,*,i]*sin(theta[j,*,i]-logical_north),rho[j,*,i]*cos(theta[j,*,i]-logical_north),color=color[i]
;    end
  end
  for i=0,3 do begin
    r1=(rho[*,0,i])[*]
    r2=(rho[169,*,i])[*]
    r3=reverse((rho[*,339,i])[*])
    r4=reverse((rho[0,*,i])[*])
    q1=(theta[*,0,i])[*]
    q2=(theta[169,*,i])[*]
    q3=reverse((theta[*,339,i])[*])
    q4=reverse((theta[0,*,i])[*])
    r=[r1,r2,r3,r4]
    q=[q1,q2,q3,q4]
    oplot,r*sin(q-logical_north),r*cos(q-logical_north),color=0,thick=2
  end
  xyouts,1100/2,-37,'PX',charsize=1.5,charthick=2
  xyouts,-1100/2,-37,align=1,'MX',charsize=1.5,charthick=2
  xyouts,0,475/2,align=0.5,'MY',charsize=1.5,charthick=2
  xyouts,0,-550/2,align=0.5,'PY',charsize=1.5,charthick=2

  device,/close
  set_plot,'x'
end
