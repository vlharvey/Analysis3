function epd_calculator, u,v,w,t,lat,p,qg_approx=qg_approx

if n_elements(qg_approx) eq 0 then qg_approx = 0

ns = size(u)
nlon = ns[1] 
nlat = ns[2]
np = ns[3]

;constants
p0 = 1e3
hscale = 7000.
rgas = 287.
cp = 1004.
gravit = 9.81
omega = 2.*!pi/86400.
aearth = 6.37e6

clat = cos(lat*!dtor)
slat = sin(lat*!dtor)
zh = hscale*alog(p0/p)
fcor = 2.*omega* slat

t0 = gravit*hscale/rgas
rho0 = p0*1e2/(rgas*t0)

rho = rho0*exp(-zh/hscale)

; calculate U,V zonal deviation
up = zonal_deviation(u)
vp = zonal_deviation(v)

; calculate W zonal deviation only if NOT using the QG-approximation
; in which case wp=0 by definition
wp = make_array(nlon,nlat,np,value=0.)
if qg_approx eq 0 then wp=zonal_deviation(w)

; calculate potential temperature, zonal average and zonal deviation
th = theta_calculator(t,p)
thza = total (th, 1) / float(nlon)
thp = zonal_deviation(th)

; calculate zonal averages of U, V, and W
uza = total(u, 1) / float(nlon)
wza = total(w, 1) / float(nlon)
vza = total(v, 1) / float(nlon)

; veritcal gradient of U, only if NOT using QG-approximation in which
; case uz=0 by definition
uz = make_array(nlat , np , value=0.)
if qg_approx eq 0 then for j=0,nlat-1 do uz[j,*] = deriv(zh , uza[j,*] )

; meridional gradient of U, only if NOT using QG-approximation in
; which case uy=0
uy = make_array(nlat,np,value=0.)
if qg_approx eq 0 then for k=0,np-1 do uy[*,k] = deriv( lat*!dtor , uza[*,k]*clat)

; calculate vertical gradient of potential temperature: thz
if qg_approx eq 1 then begin

; If using QG-approximation, then evaluate globally averaged thza
   thga = make_array(np, value=0.)
   for k=0,np-1 do thga[k] = int_tabulated(lat*!dtor , thza[*,k] * clat) / int_tabulated(lat*!dtor , clat)
   thgaz = deriv(zh , thga)
   thz = make_array(nlat,np, value=0.)
   for k=0,np-1 do thz[*,k] = thgaz[k]

endif else if qg_approx eq 0 then begin

; if not using QG-approximation, evaluate at each point
   thz = make_array(nlat,np , value=0.)
   for j=0,nlat-1 do thz[j,*] = deriv(zh , thza[j,*] )
 
endif

; standard calculation of eddies covariances and their zonal average
; VTH
dummy = vp*thp & vth = total(dummy, 1) / float(nlon)

; UV
dummy = up*vp & uv = total(dummy, 1) / float(nlon)

; UW
dummy = up*wp & uw = total(dummy, 1) / float(nlon)

; Fy
fy = (-uv + uz*vth/thz)

; Fz
fz = make_array(nlat,np, value=0.)
for j=0,nlat-1 do  fz[j,*] = -uw[j,*]  $
   + ( fcor[j] - uy[j,*] / ( aearth * clat[j] ) ) * vth[j,*] / thz[j,*]

for k=0,np-1 do begin

   fy[*,k] = fy[*,k] * rho[k] * aearth * clat
   fz[*,k] = fz[*,k] * rho[k] * aearth * clat

endfor                          ; k

; meridional derivs of Fy
dfy = make_array(nlat,np, value=0.)
for k=0,np-1 do begin
   dfy[*,k] = deriv(lat*!dtor , fy[*,k]*clat)
   dfy[*,k] = dfy[*,k] /(aearth*clat)
endfor                          ; k

; vertical derivs of Fz
dfz = make_array(nlat,np, value=0.)
for j=0,nlat-1 do dfz[j,*] = deriv(zh,fz[j,*])


; del(f) ... finally!
epd = make_array(nlat,np,value=0.)
for k=0,np-1 do epd[*,k] = ( dfy[*,k] + dfz[*,k] ) / ( rho[k] * aearth * clat )


; heat flux and WSTAR
hfx = make_array(nlat,np, value=0.)
wstar = make_array(nlat,np, value=0.)
for k=0,np-1 do begin

   hfx[*,k] = vth[*,k] / thz[*,k]
   
   wstar[*,k] = deriv(lat*!dtor, clat * hfx[*,k] )
   wstar[*,k] = wza[*,k] + wstar[*,k] / (aearth * clat)

endfor                          ;k

; VSTAR
vstar = make_array(nlat,np, value =0.)
for j=0,nlat-1 do begin

   vstar[j,*] = deriv(zh,rho * hfx[j,*] )
   vstar[j,*] = vza[j,*] - vstar[j,*] / rho

endfor                          ; j

; build output array:
; 0 is EPD
; 1 is Fy
; 2 is Fz
; 3 is VSTAR
; 4 is WSTAR
vo = make_array(nlat,np,5, value=0.)

vo[*,*,0] = epd 
vo[*,*,1] = fy 
vo[*,*,2] = fz 
vo[*,*,3] = vstar 
vo[*,*,4] = wstar

return,vo


end

function zonal_deviation, v

ns = size(v)

; zonal average
b = total(v,1,/nan) / float(ns[1])

; add degenerate dimension
b = reform(b, 1, ns[2], ns[3] )

; rebinning
c = rebin( b, ns[1], ns[2], ns[3] )

; zonal deviation
vd = v - c

return, vd

end


function theta_calculator, t,p, p_is_true=p_is_true

if n_elements(p_is_true) eq 0 then p_is_true = 1

;constants
p0=1e3
hscale=7000.
rgas=287.
cp=1004.
gravit=9.81
omega=2.*!pi/86400.
aearth=6.37e6
kappa = rgas / cp
ns=size(t)

nlon = ns[1] & nlat = ns[2] & np = ns[3]

if p_is_true eq 1 then begin

; input vertical coordinate is pressure in hPa
   pp=reform(p,1,1,np)
   p3d=rebin(pp,nlon,nlat,np)
   pp=0.
   th=t*(1e3/p3d)^(rgas/cp)

endif else if p_is_true eq 0 then begin

; input vertical coordinate is height in km
   pp=reform(p,1,1,np)
   p3d=rebin(pp,nlon,nlat,np)
   pp=0.
   th = t * exp( kappa * p3d *1e3 / hscale)

endif


return,th

end
