pro recalc_lat_1b, l1b

                junk=where(tag_names(l1b) eq 'HEMISPHERE',count)
                if count gt 0 then hem=l1b.hemisphere else hem='N'
                level_1b_lat_lon,l1b,lat=lat,lon=lon,south=(hem eq 'S')
                junk=where(tag_names(l1b) eq 'LATITUDE', count)
                if count eq 0 then begin
                  l1b=create_struct(l1b,'latitude',ptr_new(lat),'longitude',ptr_new(lon))
                end else begin
                  str_free,l1b.latitude
                  str_free,l1b.longitude
                  l1b.latitude=ptr_new(lat)
                  l1b.longitude=ptr_new(lon)
                end
end
  
  
  
  
  
  
