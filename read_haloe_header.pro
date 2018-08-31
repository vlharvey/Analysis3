pro read_haloe_header,iunit,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
charexp='                                 '
ukmo=' '
nmc=' '
ecmwf=' '
restart=' '
rfile='                                   '
istime=0L
ictime=0L
dirw='                                    '
dirs='                                    '
nfiles=0L
wfile='                                   '
sfile='                                   '
dtflow=0.
dtout=0.
dt=0.
igw=' '
ofile='                                   '
nr=0L
nc=0L
time=0.
readu,iunit,charexp
readu,iunit,ukmo
readu,iunit,nmc
readu,iunit,ecmwf
readu,iunit,restart
readu,iunit,rfile
readu,iunit,istime
readu,iunit,dirw
readu,iunit,nfiles
wfiles=strarr(nfiles)
for n=0,nfiles-1 do begin
    readu,iunit,wfile
    wfiles(n)=wfile
endfor
readu,iunit,dirs
readu,iunit,nfiles
sfiles=strarr(nfiles)
for n=0,nfiles-1 do begin
    readu,iunit,sfile
    sfiles(n)=sfile
endfor
readu,iunit,ictime
readu,iunit,dtflow
readu,iunit,dt
readu,iunit,igw
readu,iunit,dtout
readu,iunit,ofile
nfiles=long(dtflow/dtout)*(nfiles-1)
return
end
