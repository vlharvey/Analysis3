pro aura2date,ictime,etime
;
; return date in yyyymmddhh format given AURA time
; is in elapsed seconds since midnight 1 Jan 1993; 
; i.e.  HIRDLS etime=86400. gives ictime=1993010100
;
mno=[31,28,31,30,31,30,31,31,30,31,30,31]
istime=1993010100L
ntime=n_elements(etime)
ictime=lonarr(ntime)
ehr=etime/60./60.	; convert etime from seconds to hours
for n=0L,ntime-1L do begin
    yy1=istime/1000000
    if yy1 mod 4 eq 0 then mno(1)=29L
    if yy1 mod 4 ne 0 then mno(1)=28L
    mm1=istime/10000L-yy1*100L
    dd1=istime/100L-yy1*10000L-mm1*100L
    dd2=dd1+long(ehr(n))/24L
    hh1=istime-yy1*1000000L-mm1*10000L-dd1*100L
    yy2=yy1 & mm2=mm1 & hh2=hh1
    while dd2 gt mno(mm2-1) do begin
          dd2=dd2-mno(mm2-1)
          mm2=mm2+1L
          if mm2 gt 12L then begin
             mm2=mm2-12L
             yy2=yy2+1L
             if yy2 mod 4 eq 0 then mno(1)=29
             if yy2 mod 4 ne 0 then mno(1)=28
          endif
    endwhile
    hh2=long(ehr(n) mod 24)
    if hh2 gt 24L then begin
       hh2=hh2-24L
       dd2=dd2+1L
       if dd2 gt mno(mm2-1L) then begin
          dd2=dd2-mno(mm2-1L)
          mm2=mm2+1L
          if mm2 gt 12L then begin
             mm2=mm2-12L
             yy2=yy2+1L
          endif
       endif
    endif
    ictime(n)=hh2+dd2*100L+mm2*10000L+yy2*1000000L
;   print,ehr(n),yy2,mm2,dd2,hh2,ictime(n)
endfor
index=where(etime eq -999.99)
if index(0) ne -1L then ictime(index)=-99L
return
end
