;Create multiple px correction maps "Chris flats" based on v3.12 standard data
;  inputs
;    orbits - scalar or array of orbit numbers to make maps of
;    path   - path to write px map to, defaults to ~/aim
;  output
;    a file called <path>/px_map_<orbit>.sav for each orbit you request
pro multi_map_px,orbits,path=path,sza_min=sza_min,_extra=extra
;for each orbit
  if n_elements(path) eq 0 then path=string(getenv('HOME'),format='(%"%s/aim/data/cips/v3.16px_block")')
  for i=0,n_elements(orbits)-1 do begin
    ;run map_px
    for j=0,n_elements(sza_min)-1 do begin
      fn=string(path,orbits[i],sza_min[j],sza_min[j]+10,format='(%"%s/px_map_%05d_range_%d_%d.sav")')
      print,"Saving orbit ",orbits[i]," px map to ",fn
      px_map=map_px_mx(orbits[i],succeed=succeed,sza_range=[sza_min[j],sza_min[j]+10],_extra=extra,retain=retain)
      if succeed then save,px_map,filename=fn
    end
    delete_var,retain
  end
end
