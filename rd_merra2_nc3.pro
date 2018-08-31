pro rd_merra2_nc3,file1,nlg,nlat,nth,alon,alat,thlev,$
    ipv,prs,u,v,qdf,mark,qv,z,sf,q,o3,iflg
iflg=0
dum1=findfile(file1)
if dum1(0) ne '' then begin
   ncid=ncdf_open(file1)
;  print,'opening ',file1
   goto,jump
endif
if dum1(0) eq '' then begin
   iflg=1
   return
endif
stop
jump:
ncdf_diminq,ncid,0,name,nlat
ncdf_diminq,ncid,1,name,nlg
ncdf_diminq,ncid,2,name,nth
alon=fltarr(nlg)
alat=fltarr(nlat)
thlev=fltarr(nth)
ipv=fltarr(nlat,nlg,nth)
prs=fltarr(nlat,nlg,nth)
u=fltarr(nlat,nlg,nth)
v=fltarr(nlat,nlg,nth)
qdf=fltarr(nlat,nlg,nth)
mark=fltarr(nlat,nlg,nth)
qv=fltarr(nlat,nlg,nth)
z=fltarr(nlat,nlg,nth)
sf=fltarr(nlat,nlg,nth)
q=fltarr(nlat,nlg,nth)
o3=fltarr(nlat,nlg,nth)
ncdf_varget,ncid,0,alon
ncdf_varget,ncid,1,alat
ncdf_varget,ncid,2,thlev
ncdf_varget,ncid,3,ipv
ncdf_varget,ncid,4,prs
ncdf_varget,ncid,5,u
ncdf_varget,ncid,6,v
ncdf_varget,ncid,7,qdf
ncdf_varget,ncid,8,mark
ncdf_varget,ncid,9,qv
ncdf_varget,ncid,10,z
ncdf_varget,ncid,11,sf
ncdf_varget,ncid,12,q
ncdf_varget,ncid,13,o3
ncdf_close,ncid
end
