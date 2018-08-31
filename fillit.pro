pro fillit,cn,cnfill
result=size(cn)
nc=result(1)
nr=result(2)
cnfill2=cn
for i=0,nc-1 do begin
    dummy=reform(cnfill2(i,*),nr)
    index1=where(dummy gt 0.,ngood)
    index2=where(dummy le 0.)
    if ngood gt 1 and index1(0) ne -1 and index2(0) ne -1 then begin
       filled=interpol(dummy(index1),index1,index2)
       cnfill2(i,index2)=filled
    endif
endfor
cnfill1=cnfill2
for j=0,nr-1 do begin
    dummy=reform(cnfill1(*,j),nc)
    index1=where(dummy gt 0.,ngood)
    index2=where(dummy le 0.,nbad)
    if ngood gt 1L and nbad gt 1L then begin
       filled=interpol(dummy(index1),index1,index2)
       cnfill1(index2,j)=filled
    endif
endfor
cnfill=cnfill1
return
end
