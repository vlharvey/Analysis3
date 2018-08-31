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
pro tile_microscope2,orbit,retain=retain,path_new=path_new,path_original=path_original
  if n_elements(retain) eq 0 then begin
    if n_elements(path_original) eq 0 then path_original='/aim/data/cips/v3.20/south_2008'
    l4_original=read_cips_file_bynum(path_original+'/level_4','4',orbit)
;    l4_new=read_cips_file_bynum(path_new,'4',orbit)
    l1b_original=read_cips_file_bynum(path_original+'/level_1b','1b',orbit,/by_image)
;    l1b_new=read_cips_file_bynum(path_new+'/level_1b','1b',orbit,/by_image)
    tic
    tiles_original=make_tiles(l1b_original,ret_info=default_ret_info())
    toc,"Made tiles original"
;    tiles_new=make_tiles(l1b_new,ret_info=default_ret_info())
;    toc,"Made tiles new"
    l1b_new=0
    l4_new=0
    tiles_new=0
    retain={l4_new:l4_new,l4_original:l4_original, $
            l1b_original:l1b_original, $
            tiles_original:tiles_original,l1b_new:l1b_new,tiles_new:tiles_new} 
  end
  cid=dereference_single(retain.l1b_original.camera_id)
;    s=size(*retain.l4_new.indicators.ratall,/dim)
;    ratall_no_nadir=dblarr(s)
;    for x=0,s[0]-1 do begin
;      for y=0,s[1]-1 do begin
;        f=finite(retain.tiles_new.alb[x,y,*])
;        f_no_nadir=logical_and(logical_and(f,cid ne 'my'),cid ne 'py')
;        w_no_nadir=where(f_no_nadir,count_no_nadir)
;        if count_no_nadir gt 0 then begin
;          alb_nn=retain.tiles_new.alb[x,y,w_no_nadir]
;          auc_nn=alb_nn*0+1
;          ssa_nn=retain.tiles_new.ssa[x,y,w_no_nadir]
;          cva_nn=retain.tiles_new.cva[x,y,w_no_nadir]
;          sza_nn=retain.tiles_new.sza[x,y,w_no_nadir]
;          calc_indicators_core,alb=alb_nn,auc=auc_nn,ssa=ssa_nn,cva=cva_nn,sza=sza_nn,ratall=ratall_nn,good=good
;          ratall_no_nadir[x,y]=ratall_nn
;        end
;      end
;    end
;    toc,"indicators no nadir"
  device,decompose=0
  loadct,39
  flat_albedo=alog10(*retain.l4_original.ozone_col_density)>0
  print,max(flat_albedo,/nan)
  ;Display the number of layers
  wset,0
  tvscl,flat_albedo,/nan
  s=size(flat_albedo,/dim)
;  tvscl,rebin(ratall_no_nadir>0.998<1.002,s[0]*2,s[1]*2,/sample),/nan
  print,"Ready for action!"
  cursor,x,y,/up,/device
  x_old=-1
  y_old=-1
  while !mouse.button ne 2  do begin
    if x gt 0 and y gt 0 and x lt s[0] and y lt s[1] and (x ne x_old or y ne y_old) then begin
      ;Plot a scattering profile from the point the user clicked
      f=finite(retain.tiles_original.alb[x,y,*])
      w=where(f,count)
      if count gt 0 then begin
        alb=retain.tiles_original.alb[x,y,w]
        auc=alb*0+1
        ssa=retain.tiles_original.ssa[x,y,w]
        cva=retain.tiles_original.cva[x,y,w]
        sza=retain.tiles_original.sza[x,y,w]
        wset,1
        plot,ssa,alb,psym=-1,xrange=[0,180],/xs
        oplot,cva,alb,psym=-1,color=254
        xyouts,ssa,alb,cid[w],charsize=2
        xyouts,cva,alb,cid[w],charsize=2,color=254
        wset,3
        plot,*retain.l4_original.zenith_angle_ray_peak,*retain.l4_original.indicators.ratall,yrange=[0.99,1.01],/ys,psym=3
;        oplot,*retain.l4_new.zenith_angle_ray_peak,*retain.l4_new.indicators.ratall,psym=3,color=64
;        oplot,*retain.l4_new.zenith_angle_ray_peak,ratall_no_nadir,psym=3,color=254
        oplot,[(*retain.l4_original.zenith_angle_ray_peak)[x,y]],[(*retain.l4_original.indicators.ratall)[x,y]],psym=1,symsize=4,color=254
;        oplot,[(*retain.l4_new.zenith_angle_ray_peak)[x,y]],[(*retain.l4_new.indicators.ratall)[x,y]],psym=1,symsize=4,color=192
;        oplot,[(*retain.l4_new.zenith_angle_ray_peak)[x,y]],[(ratall_no_nadir)[x,y]],psym=1,symsize=4,color=128
      
      end
      f=finite(retain.tiles_original.alb[x,y,*])
      w=where(f,count)
      if count gt 0 then begin
        alb=retain.tiles_original.alb[x,y,w]
        auc=alb*0+1
        ssa=retain.tiles_original.ssa[x,y,w]
        cva=retain.tiles_original.cva[x,y,w]
        sza=retain.tiles_original.sza[x,y,w]
        wset,2
        calc_xy,alb,ssa,cva,sza,data_x=data_x,data_y=data_y
        calc_indicators_core,alb=alb,auc=auc,ssa=ssa,cva=cva,sza=sza,ratall=ratall,good=good

        plot,data_x,data_y,/ynoz
        xyouts,data_x,data_y,cid[w],charsize=2
        xyouts,/normal,0.1,0.25,string(good,format='(%"Good=%d")'),charsize=2
        fresh_csigma=calc_one_nocloud_parm_lin(ssa,cva,sza,alb,fit=fit)
        oplot,data_x,poly(data_x,fit),psym=-3
        if good then begin
          xyouts,/normal,0.1,0.2,string(ratall,format='(%"Ratall   =%f")'),charsize=2
          xyouts,/normal,0.1,0.3,string(10d^flat_albedo[x,y],format='(%"stored C=%f")'),charsize=2
          xyouts,/normal,0.1,0.4,string(fresh_csigma[0],format='(%"fresh C=%f")'),charsize=2

        end
      end
    end
    wait,0.1
    ;Ask the user to click on the image, and get the coordinates of the click
    wset,0
    x_old=x
    y_old=y
    cursor,x,y,/device,/nowait
  end
end
