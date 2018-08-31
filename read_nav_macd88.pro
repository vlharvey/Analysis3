function read_NAV_macd88,filename,lat=lat,lon=lon,p=p,yyyymmddhh=yyyymmddhh

;
; Function for reading Karl's special nogaps/navdas files
;
;


   nlon=0L & nlat=0L & np=0L & yyyy=0L & mm=0L & dd=0L & hh=0L
   spare1=0L & spare2=0L & spare3=0L

   openr,lun,filename,/get_lun
   readu,lun,nlon,nlat,np,yyyy,mm,dd,hh,spare1,spare2,spare3    

   lon = fltarr(nlon)            ;longitude grid
   lat = fltarr(nlat)            ;latitude grid
   p   = fltarr(np)              ;pressure grid
   v   = fltarr(nlon,nlat,np)    ;field array

   readu,lun,lon
   readu,lun,lat
   readu,lun,p    
   readu,lun,v
   close,lun
   free_lun,lun

   yyyymmddhh = string(yyyy,format='(I04)') $
              + string(mm,format='(I02)')   $
              + string(dd,format='(I02)')   $
              + string(hh,format='(I02)')

   return,v

end
