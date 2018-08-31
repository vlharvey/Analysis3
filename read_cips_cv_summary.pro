; =====================================================================================
  pro read_cips_cv_summary,file,npix,nrev,rev,year,doy,ut,ltime,ncv,ncld,cld_presence, $
                           cld_frac,lat,lon,sza,radius,albedo,iwc,dist,cld_map
; =====================================================================================
; Reads in the CIPS common volume summary files (ascii) and returns all data. 
; -------------------------------------------------------------------------------------
; Input:
;
; file     Name of ascii file to be read (includes full path)
;
; Output:
;
;         npix - integer           Maximum number of CIPS pixels in the common volume.
;         nrev - integer           Number of AIM orbits in the season.
;          rev - intarr(nrev)      AIM orbit number.
;         year - intarr(nrev)      Year.
;          doy - intarr(nrev)      Day of year.
;           ut - fltarr(nrev)      Mean UT time of CIPS CV measurements.
;        ltime - fltarr(nrev)      Mean local time of CIPS CV measurements.
;          ncv - intarr(nrev)      Actual number of CIPS pixels in the common volume.
;         ncld - intarr(nrev)      Number of CIPS pixels containing clouds.
; cld_presence - intarr(nrev)      = 1 clouds detected in CIP common volume
;                                  = 0 no clouds detected in CIP common volume
;     cld_frac - fltarr(nrev)      Fraction of CIPS pixels containing clouds.
;          lat - fltarr(nrev,npix) Latitude of each CIPS CV pixel [deg].
;          lon - fltarr(nrev,npix) Longitude of each CIPS CV pixel [deg].
;          sza - fltarr(nrev,npix) Solar zenith angle of each CIPS CV pixel [deg].
;         dist - fltarr(nrev,npix) Distance of CIPS CV pixel from SOFIE occ point [km].
;      cld_map - intarr(nrev,npix) Cloud pixel map (1 for cloud pixel, 0 for no-cloud)
;       albedo - fltarr(nrev,npix) Cloud albedo [1.e-6 sr-1].
;       radius - fltarr(nrev,npix) Cloud particle radius [nm].
;          iwc - fltarr(nrev,npix) Vertical column ice water content [ug/m2].
;
;  Data default values:
;                           0.00    indicates no cloud present (radius/albedo/iwc)
;                          -1.00    indicates pixel is not in common volume.
; -------------------------------------------------------------------------------------
default=-1.0
str=' '

npix=35

; Find length of file

get_lun,v
openr,v,file
i=0L
while(not eof(v)) do begin
  readf,v,str
  i+=1
endwhile
close,v
free_lun,v
ntot=i-3

; # of orbits 
nrev=ntot/(npix+1)

get_lun,u
openr,u,file
for i=1,3 do readf,u,str

rev=intarr(nrev)
year=intarr(nrev)
doy=intarr(nrev)
 ncv=intarr(nrev)
ncld=intarr(nrev)
cld_presence=intarr(nrev)
cld_frac=fltarr(nrev)
ut=fltarr(nrev)
ltime=fltarr(nrev)
cld_map=intarr(nrev,npix)
lat=fltarr(nrev,npix)
lon=fltarr(nrev,npix)
sza=fltarr(nrev,npix)
radius=fltarr(nrev,npix)
albedo=fltarr(nrev,npix)
iwc=fltarr(nrev,npix)
dist=fltarr(nrev,npix)

i1=1 & i2=1 & i3=1 & i4=1 & i5=1 & i6=1
x1=1.0 & x2=1.0 & x3=1.0

ncol=7

xx=fltarr(ncol,npix)
for i=0,nrev-1 do begin

   readf,u,i1,i2,i3,x1,x2,i4,i5,i6,x3
   if(i4 gt npix) then begin
     print,'Too many pixels being read in - increase parameter npix'
     stop
   endif

   rev(i)=i1
   year(i)=i2
   doy(i)=i3
   ut(i)=x1
   ltime(i)=x2
   ncv(i)=i4
   cld_presence(i)=i5
   ncld(i)=i6
   cld_frac(i)=x3

   readf,u,xx

      lat(i,*)=reform(xx(0,*))
      lon(i,*)=reform(xx(1,*))
      sza(i,*)=reform(xx(2,*))
     dist(i,*)=reform(xx(3,*))
   radius(i,*)=reform(xx(4,*))
   albedo(i,*)=reform(xx(5,*))
      iwc(i,*)=reform(xx(6,*))

   nocld=where(albedo(i,*) eq 0. and radius(i,*) eq 0. and iwc(i,*) eq 0.,complement=cld)
   if(nocld[0] ne -1) then cld_map(i,nocld)=0
   if(cld[0] ne -1) then cld_map(i,cld)=1
   if(ncv(i) ne npix) then cld_map[i,ncv(i):npix-1]=-1
endfor
close,u
free_lun,u

return
end

