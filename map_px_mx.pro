;Demonstration of stack compaction functions
;  input
;    l1b - A level 1b structure, as read by read_cips_file
;  keyword in/out: All these are optional. If you pass in undefined variables,
;  the function will read l1b and fill in the variable. If you pass in defined
;  variables, the function will not try to process l1b. This way if you repeatedly
;  call the function with the same variables each time, the function will compact
;  l1b the first time, then not have to subsequently

;  IDL> stack_microscope,l1b,ssa=ssa,albedo=albedo,n_layers=n_layers ; This one will be quite slow,
;                                                                    ; as it has to compact the stack
;  IDL> stack_microscope,l1b,ssa=ssa,albedo=albedo,n_layers=n_layers ; This one will be much faster,
;                                                                    ; as it doesn't have to compact the stack
;    ssa - compacted scattering angles
;    albedo - compacted albedoes
;    n_layers - count of number of layers at each point
function map_px_mx,orbit,retain=retain,path=path,succeed=succeed,sza_range=sza_range,graphics=graphics
  if n_elements(sza_range) eq 0 then sza_range=[0,75]
  if n_elements(retain) eq 0 then begin
    if n_elements(path) eq 0 then path='/aim/data/cips/v3.12'
    l4=read_cips_file_bynum(path,'4',orbit,succeed=succeed)
    if ~succeed then return,0
    l1b=read_cips_file_bynum(path,'1b',orbit,/by_image,succeed=succeed)
    if ~succeed then return,0
    l1a_px=read_cips_file_bynum(path,'1a',orbit,'px',succeed=succeed)
    if ~succeed then return,0
    l1a_mx=read_cips_file_bynum(path,'1a',orbit,'mx',succeed=succeed)
    if ~succeed then return,0
    print,"Making tiles"
    tiles=make_tiles(l1b,ret_info=default_ret_info())
    retain={l4:l4, $
            l1a_px:l1a_px, $
            l1a_mx:l1a_mx, $
            l1b:l1b, $
            tiles:tiles } 
  end
  heap_gc
  if keyword_set(graphics) then begin
    device,retain=2,decompose=0
    loadct,39
    window,0
    window,1
    window,2
    window,3
  end
  flat_albedo=*retain.l4.cld_albedo
  flat_sza=*retain.l4.zenith_angle_ray_peak
  s=size(flat_albedo,/dim)
  px_quality=make_array(s[0],s[1],3,value=!values.f_nan)
  mx_quality=make_array(s[0],s[1],3,value=!values.f_nan)
  px_map=dblarr(170,340)*!values.f_nan
  px_num=intarr(170,340)
  mx_map=dblarr(170,340)*!values.f_nan
  mx_num=intarr(170,340)
  print,string(orbit,retain.l4.hemisphere,format='(%"Orbit %05d, hemisphere %s")')
  if retain.l4.hemisphere eq 'N' then begin
    w=where(abs(*retain.l4.latitude) gt 90,count)
    if count gt 0 then (*retain.l4.latitude)[w]=180d -(*retain.l4.latitude)[w]
  end else begin
    w=where(abs(*retain.l4.latitude) gt 90,count)
    if count gt 0 then (*retain.l4.latitude)[w]=-180d -(*retain.l4.latitude)[w]
  end
  ;Display the number of layers
  all_cid=dereference_single(retain.l1b.camera_id)
  yy=(rebin(transpose(indgen(340)),170,340))
  for x=0,s[0]-1 do begin
    for y=0,s[1]-1 do begin
      ;Plot a scattering profile from the point the user clicked
      w=where(finite(retain.tiles.alb[x,y,*]),count)
      if count gt 0 and flat_sza[x,y] gt sza_range[0] and flat_sza[x,y] lt sza_range[1] then begin
        alb=retain.tiles.alb[x,y,w]
        auc=alb*0+1
        ssa=retain.tiles.ssa[x,y,w]
        cva=retain.tiles.cva[x,y,w]
        sza=retain.tiles.sza[x,y,w]
        cid=all_cid[w]
        px=where(cid eq "px",px_count)
        mx=where(cid eq "mx",mx_count)
        calc_xy,alb,ssa,cva,sza,data_x=data_x,data_y=data_y
        if mx_count ge 2 then begin
          mx_x=data_x[mx]
          mx_y=data_y[mx]
          f=linfit(mx_x,mx_y)
          r=correlate(mx_x,mx_y)
          mx_quality[x,y,*]=[f,r^2]
          if mx_count ge 3 then begin
            mx_diff=mx_y-poly(mx_x,f)
            mx_w=w[mx]
            for i=0,n_elements(mx_w)-1 do begin
              mx_ww=mx_w[i]
              if retain.l1b.hemisphere eq 'N' then begin
                mx_i=mx_ww/4-1
              end else begin
                mx_i=(mx_ww-1)/4
              end
              mx_pix=find_pix(*retain.l1a_mx[mx_i].latitude,*retain.l1a_mx[mx_i].longitude,(*retain.l4.latitude)[x,y],(*retain.l4.longitude)[x,y],/deg)
              mx_num[mx_pix[0],mx_pix[1]]++;
              if finite(mx_map[mx_pix[0],mx_pix[1]]) then begin
                mx_map[mx_pix[0],mx_pix[1]]+=mx_diff[i] 
              end else begin
                mx_map[mx_pix[0],mx_pix[1]]=mx_diff[i]
              end
            end
          end
          if px_count ge 1 then begin
            px_x=data_x[px]
            px_y=data_y[px]
            if px_count gt 2 then begin
              f2=linfit(px_x,px_y)
              r=correlate(px_x,px_y)
              px_quality[x,y,*]=[f2,r^2]
            end
            px_diff=px_y-poly(px_x,f)
            px_w=w[px]
            for i=0,n_elements(px_w)-1 do begin
              px_ww=px_w[i]
              if retain.l1b.hemisphere eq 'N' then begin
                if px_ww le 3 then begin
                  px_i=px_ww
                end else begin
                  px_i=(px_ww-3)/4+3
                end
              end else begin
                if px_ww le 27*4 then begin
                  px_i=px_ww/4
                end else begin
                  px_i=27+(px_ww-27*4)
                end
              end
              px_pix=find_pix(*retain.l1a_px[px_i].latitude,*retain.l1a_px[px_i].longitude,(*retain.l4.latitude)[x,y],(*retain.l4.longitude)[x,y],/deg)
              px_num[px_pix[0],px_pix[1]]++;
              if finite(px_map[px_pix[0],px_pix[1]]) then begin
                px_map[px_pix[0],px_pix[1]]+=px_diff[i] 
              end else begin
                px_map[px_pix[0],px_pix[1]]=px_diff[i]
              end
            end
          end
        end
      end
    end
    if keyword_set(graphics) then begin
      wset,0
      tvscl,((px_quality[*,*,2]-0.9)>0.0)*255d/0.1d,340,0
      tvscl,px_map/px_num,/nan
      tvscl,px_num,170,0
      wset,1
      if n_elements(where(finite(px_map))) gt 10 then begin
        plot,yy,px_map/px_num,psym=3
      end
      wset,2
      tvscl,((mx_quality[*,*,2]-0.9)>0.0)*255d/0.1d,340,0
      tvscl,mx_map/mx_num,/nan
      tvscl,mx_num,170,0
      wset,3
      if n_elements(where(finite(mx_map))) gt 10 then begin
        plot,yy,mx_map/mx_num,psym=3
      end
    end    
  end
  return,{px_map:double(px_map)/double(px_num),mx_map:double(mx_map)/double(mx_num),px_quality:px_quality,mx_quality:mx_quality}
end
