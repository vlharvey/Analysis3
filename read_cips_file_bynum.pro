function read_cips_file_bynum,path,level,orbit,suffix,fn=fn,succeed=succeed,_extra=extra
  fn=get_cips_filename_bynum(path,level,orbit,suffix,succeed=succeed)
  if ~succeed then return,0
  print,fn
  return,read_cips_file(fn,/f,_extra=extra)
end
