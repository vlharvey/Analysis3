pro fillit,cn,cnfill
result=size(cn)
nc=result(1)
nr=result(2)
cnfill1=cn
;for i=0,nc-1 do begin
;    dummy=reform(cnfill1(i,*),nr)
;    index1=where(dummy ne -9999.,ngood)
;    index2=where(dummy eq -9999.)
;    if ngood gt 1 and index1(0) ne -1 and index2(0) ne -1 then begin
;       filled=interpol(dummy(index1),index1,index2)
;       cnfill1(i,index2)=filled
;    endif
;endfor
cnfill2=cn
for j=0,nr-1 do begin
    dummy=reform(cnfill2(*,j),nc)
    index1=where(dummy ne 9999.,ngood)
    index2=where(dummy eq 9999.)
    if ngood gt 1 and index1(0) ne -1 and index2(0) ne -1 then begin
       filled=interpol(dummy(index1),index1,index2)
       cnfill2(index2,j)=filled
    endif
endfor
;cnfill=.5*(cnfill1+cnfill2)
cnfill=cnfill2
;
; remove bad values introduced by interpol
;
;index=where(cnfill1 lt 0. or cnfill2 lt 0.)
;if index(0) ne -1 then cnfill(index)=-9999.
return
end
