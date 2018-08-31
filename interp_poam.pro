pro interp_poam,ilev,llev,dummy1,dummy2,xtheta,theta_lev
scale=0.
xtheta=-99.
;
; if desired theta surface is above highest allowable theta or
; is below lowest allowable theta then keep xtheta=-99.
;
if max(dummy2) lt theta_lev or min(dummy2) gt theta_lev then return
for l=ilev,llev-1 do begin
    if dummy1(l-1) ne -999. and dummy1(l) ne -999. and $
       dummy1(l-1) ne  -99. and dummy1(l) ne  -99. and $
       dummy1(l-1) ne 9999. and dummy1(l) ne 9999. and $
       dummy2(l-1) ne -999. and dummy2(l) ne -999. and $
       dummy2(l-1) ne  -99. and dummy2(l) ne  -99. and $
       dummy2(l-1) ne 9999. and dummy2(l) ne 9999. then begin
       if (dummy2(l-1) le theta_lev) AND $
          (dummy2(l  ) gt theta_lev) then begin
          scale=(theta_lev-dummy2(l))/(dummy2(l-1)-dummy2(l))
          xtheta=dummy1(l)+scale*(dummy1(l-1)-dummy1(l))
          return
       endif
    endif
endfor
return
end
