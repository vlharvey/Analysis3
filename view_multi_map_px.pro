pro view_multi_map_px,orbits
  device,decompose=0
  device,retain=2
  loadct,0
;for each orbit
  window,0
  window,1
  for i=0,n_elements(orbits)-1 do begin
    ;run map_px
    fn=string(orbits[i],format='(%"px_map_%05d.sav")')
    if file_test(fn) then begin
      oi=get_orbit_info_bynum(orbits[i])
      restore,fn
      if size(px_map,/type) eq 8 then px_map=px_map.px_map
      wset,0
      erase
      px_map_smooth=exp(real_smooth_img(px_map,5))
      w=where(~finite(px_map_smooth),count)
      if count ne 0 then px_map_smooth[w]=1.0

      tv,linterp(-0.05,0.0,0.05,255.0,px_map)
    
      tv,linterp(0.95,0.0,1.05,255.0,px_map_smooth),170,0
;      tvcontour,px_map_smooth,170,0,0.01,color=0
     
      xyouts,10,350,string(orbits[i],usec2vms(oi.start_time),format='(%"%4d %s")'),/device
      yy=(rebin(transpose(indgen(340)),170,340))
      wset,1
      plot,yy,px_map,psym=3,yrange=[-0.05,0.05]
      oplot,yy,px_map_smooth-1.0,psym=3,color=64
      wait,0.5
    end
  end
end
