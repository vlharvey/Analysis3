function get_cips_filename_bynum,path,level,orbit,suffix,succeed=succeed
  succeed=0
  level=strlowcase(level)
  if n_elements(suffix) eq 0 then suffix=''
  if level eq '3a' then begin
    fmt=string(path,level,orbit/1000,orbit mod 1000,suffix,format='(%"%s/cips_sci_%s_*%04d-%03d*%s*.nc*")')
  end else begin
    fmt=string(path,level,orbit,suffix,format='(%"%s/cips_sci_%s_orbit_%05d_*%s*.nc*")')
  end
  fn=file_search(fmt,count=count)
  if count eq 0 then begin
    print,"Didn't find any files like "+fmt
    return,0
  end
  succeed=1
  fn=fn[0]
  return,fn
end
