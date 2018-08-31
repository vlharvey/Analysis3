pro rd_dial_v2,dird,iyr,imn,idy,xdial,ydial,tdial,y2dial,t2dial,$
    o3dial,pdial,thdial,pvdial,udial,vdial,qdial,msfdial,qdfdial,$
    mrkpv_dial,mrksf_dial,vpdial,sfdial,nx,ny

; procedure to read the augmented DIAL curtain file

ifile=dird+'dial'+string(FORMAT='(I4)',iyr)+$
      string(FORMAT='(I2.2)',imn)+$
      string(FORMAT='(I2.2)',idy)+'.diag'
close,2
openr,2,ifile

idate=0L
readf,2,idate
print,' idate =',idate
nx=0L
ny=0L
readf,2,nx
xdial=fltarr(nx)
ydial=fltarr(nx)
tdial=fltarr(nx)
readf,2,xdial,ydial,tdial

readf,2,nx,ny
o3dial=fltarr(nx,ny)
pdial=fltarr(nx,ny)
thdial=fltarr(nx,ny)
pvdial=fltarr(nx,ny)
udial=fltarr(nx,ny)
vdial=fltarr(nx,ny)
qdial=fltarr(nx,ny)
msfdial=fltarr(nx,ny)
qdfdial=fltarr(nx,ny)
mrkpv_dial=fltarr(nx,ny)
mrksf_dial=fltarr(nx,ny)
vpdial=fltarr(nx,ny)
sfdial=fltarr(nx,ny)

readf,2,o3dial,pdial,thdial,pvdial,udial,vdial,$
        qdial,msfdial,qdfdial,mrkpv_dial,mrksf_dial,$
        vpdial,sfdial
close,2
t2dial=fltarr(nx,ny)
y2dial=fltarr(nx,ny)
for k = 0, nx-1 do begin
  y2dial(k,*) = ydial(k)
  t2dial(k,*) = tdial(k)
endfor

return
end
