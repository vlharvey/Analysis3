pro tropopause,t,p,z,th,nl,ptrop,ztrop,thtrop
;
; assume you have t and p soundings of dimension nl
; assume that t and p arrays are top down
;
; compute lapse rate
;
; dT/dz=-(g/RT)*dT/dlnp

R=287.05
g=9.8
laps=0*t
for l=1,nl-1 do begin
    lm1=l-1
    dt=t(l)-t(lm1)
    pl=p(l)
    plm1=p(lm1)
    dlnp=alog(pl/plm1)
    laps(l)=-(g/(t(l)*R))*dt/dlnp
endfor
;
; compute Jacobian
;
dpdth=0*t
for l=0,nl-1 do begin
    lm1=l-1
    lp1=l+1
    if l eq 0 then lm1=l
    if l eq nl-1 then lp1=l
    dth=th(lp1)-th(lm1)
    dp=p(lp1)-p(lm1)
    dpdth(l)=dp/dth
endfor
;
; laps(1) is top
;
ptrop=9999. 
ztrop=9999.
thtrop=9999.
;sounding=reform(laps(1:nl-5),nl-5)
;p_sub=reform(p(1:nl-5),nl-5)
;z_sub=reform(z(1:nl-5),nl-5)
;th_sub=reform(th(1:nl-5),nl-5)
sounding=laps
p_sub=p
z_sub=z
th_sub=th
tindex=0
;
; tropopause is where lapse rate is less than 2K/km
; exclude lowest 4 layers to account for sfc inversions
;
index=where(sounding ge -2.e-3)
if index(0) ne -1 then begin
   npnt=n_elements(index)

; check from bottom-up for 2 levels next to each other
   for n=npnt-1,1,-1 do begin
       if index(n-1)-index(n)+1l eq 0l then begin
          tindex=index(n)
          goto, jumpout
       endif
   endfor
   jumpout:
endif
;
; tindex is lower edge of lapse rate
; tindex -1 is upper edge of lapse rate
;
if tindex ne 0 then begin
   ptrop=p_sub(tindex)
   ztrop=z_sub(tindex)
   thtrop=th_sub(tindex)
endif
return
end


