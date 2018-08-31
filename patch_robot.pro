pro patch_robot,level,in_path,in_version,out_path,out_version,robot_pro,_extra=extra
  set_cips_production_vars
  in_filter=string(format='(%"%s/cips_sci_%s_orbit_*_v%03d.nc")',in_path,level,in_version)
  print,"Searching for ",in_filter
  inf=file_search(in_filter,count=count)
  print,count," files found"
  tic
  for i=0,n_elements(inf)-1 do begin
    print,"In file:  ",inf[i]
    ouf=strrep(inf[i],string(format='(%"_v%03d")',in_version),string(format='(%"_v%03d")',out_version),/regex)
    ouf=out_path+file_basename(ouf)
    data=read_cips_file(inf[i],/f,_extra=extra)
    call_procedure,robot_pro,data,_extra=extra
    write_cips_file,data,ouf,data_level=level,/net_cdf
    toc,"Out file: "+ouf
    heap_gc
  end
end
