;
; store monthly mean
;
@rd_sdwaccm4_nc3
@write_sdwaccm4_nc3

loadct,39
mcolor=byte(!p.color)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
nxdim=700
nydim=700
xorig=[0.15]
yorig=[0.25]
xlen=0.7
ylen=0.5
device,decompose=0
mcolor=byte(!p.color)
nlvls=20L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
smon=['01','02','03','04','05','06','07','08','09','10','11','12']
nmon=n_elements(smon)
set_plot,'ps'
setplot='ps'
read,'setplot= ',setplot
if setplot ne 'ps' then begin
   set_plot,'x'
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=255
endif
;
; get file listing
;
dirw='/Volumes/cloud/data/WACCM_data/Datfiles_SD/'

for imon=0L,nmon-1L do begin
ifiles=file_search(dirw+'f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.h5.????'+smon(imon)+'??.nc3',count=nfile)
;
; loop over files
;
icount=0L
FOR n=0l,nfile-1l DO BEGIN
    result=strsplit(ifiles(n),'.',/extract)
    sdate=result(4)
    print,sdate
    ifile=dirw+'f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.h5.'+sdate+'.nc3'
    rd_sdwaccm4_nc3,ifile,nc,nr,nth,alon,alat,th,$
       pv2,p2,g2,u2,v2,q2,qdf2,mark2,sf2,h2o2,n2o2,o32,iflg
    if iflg eq 1 then goto,jumpstep
;
; monthly means
;
if icount eq 0L then begin
   pv2mean=pv2
   p2mean=p2
   g2mean=g2
   u2mean=u2
   v2mean=v2
   q2mean=q2
   qdf2mean=qdf2
   mark2mean=mark2
   sf2mean=sf2
   h2o2mean=h2o2
   n2o2mean=n2o2
   o32mean=o32
endif
if icount gt 0L then begin
   pv2mean=pv2mean+pv2
   p2mean=p2mean+p2
   g2mean=g2mean+g2
   u2mean=u2mean+u2
   v2mean=v2mean+v2
   q2mean=q2mean+q2
   qdf2mean=qdf2mean+qdf2
   mark2mean=mark2mean+mark2
   sf2mean=sf2mean+sf2
   h2o2mean=h2o2mean+h2o2
   n2o2mean=n2o2mean+n2o2
   o32mean=o32mean+o32
endif

markzm=mean(mark2,dim=2)
uzm=mean(u2,dim=2)
gzm=mean(g2,dim=2)/1000.
pzm=mean(p2,dim=2)
qzm=mean(q2,dim=2)
h2ozm=mean(h2o2,dim=2)*1.e6
n2ozm=mean(n2o2,dim=2)*1.e9
tzm=0.*pzm
for k=0L,nth-1L do tzm(*,k)=th(k)*(pzm(*,k)/1000.)^.286
thzm=tzm*(1000./pzm)^0.286
erase
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
nlvls=31L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
contour,uzm,alat,gzm,levels=-150.+10.*findgen(31),/noeras,xrange=[-90.,90],yrange=[10.,110.],charsize=2,c_color=col1,/fill,title=sdate,xtitle='Latitude',ytitle='Geopotential Height (km)',xticks=6,color=0
contour,uzm,alat,gzm,levels=-150.+10.*findgen(15),/noeras,/overplot,/follow,color=mcolor,c_linestyle=5
;contour,qzm,alat,gzm,levels=-15.+findgen(15),/noeras,/overplot,/follow,color=mcolor*.9,c_linestyle=5
contour,uzm,alat,gzm,levels=10.+10.*findgen(15),/noeras,/overplot,/follow,color=0
contour,markzm,alat,gzm,levels=[0.1,0.5,0.9],/noeras,/overplot,/follow,color=0,thick=5
;contour,h2ozm,alat,gzm,levels=1.+0.5*findgen(10),/noeras,/overplot,/follow,color=250
;contour,n2ozm,alat,gzm,levels=0.1+0.5*findgen(10),/noeras,/overplot,/follow,color=250
;contour,thzm,alat,gzm,levels=reverse(th(0:nth-1:2)),/follow,thick=3,/overplot,c_annotation=strcompress(long(reverse(th(0:nth-1:2))),/remove_all)
contour,tzm,alat,gzm,levels=[150.],/follow,thick=3,/overplot,color=0.1*mcolor
contour,tzm,alat,gzm,levels=[160.],/follow,thick=3,/overplot,color=0.15*mcolor
contour,tzm,alat,gzm,levels=[170.],/follow,thick=3,/overplot,color=0.2*mcolor
contour,tzm,alat,gzm,levels=[180.],/follow,thick=3,/overplot,color=0.3*mcolor
contour,tzm,alat,gzm,levels=[190.],/follow,thick=3,/overplot,color=0.35*mcolor
contour,tzm,alat,gzm,levels=[200.],/follow,thick=3,/overplot,color=0.4*mcolor
contour,tzm,alat,gzm,levels=[210.],/follow,thick=3,/overplot,color=0.5*mcolor
contour,tzm,alat,gzm,levels=[220.],/follow,thick=3,/overplot,color=0.6*mcolor
contour,tzm,alat,gzm,levels=[230.],/follow,thick=3,/overplot,color=0.65*mcolor
contour,tzm,alat,gzm,levels=[240.],/follow,thick=3,/overplot,color=0.7*mcolor
contour,tzm,alat,gzm,levels=[250.],/follow,thick=3,/overplot,color=0.75*mcolor
contour,tzm,alat,gzm,levels=[260.],/follow,thick=3,/overplot,color=0.8*mcolor
contour,tzm,alat,gzm,levels=[270.],/follow,thick=3,/overplot,color=0.9*mcolor
contour,tzm,alat,gzm,levels=[280.],/follow,thick=3,/overplot,color=0.95*mcolor
contour,tzm,alat,gzm,levels=[290.],/follow,thick=3,/overplot,color=mcolor

icount=icount+1L
jumpstep:
endfor	; loop over files
;
; average
;
pv2mean=pv2mean/float(icount)
p2mean=p2mean/float(icount)
g2mean=g2mean/float(icount)
u2mean=u2mean/float(icount)
v2mean=v2mean/float(icount)
q2mean=q2mean/float(icount)
qdf2mean=qdf2mean/float(icount)
mark2mean=mark2mean/float(icount)
sf2mean=sf2mean/float(icount)
h2o2mean=h2o2mean/float(icount)
n2o2mean=n2o2mean/float(icount)
o32mean=o32mean/float(icount)

pv2zmean=mean(pv2mean,dim=2)
p2zmean=mean(p2mean,dim=2)
g2zmean=mean(g2mean,dim=2)/1000.
u2zmean=mean(u2mean,dim=2)
v2zmean=mean(v2mean,dim=2)
q2zmean=mean(q2mean,dim=2)
qdf2zmean=mean(qdf2mean,dim=2)
mark2zmean=mean(mark2mean,dim=2)
sf2zmean=mean(sf2mean,dim=2)
h2o2zmean=mean(h2o2mean,dim=2)
n2o2zmean=mean(n2o2mean,dim=2)
o32zmean=mean(o32mean,dim=2)

yyyymm=strmid(sdate,0,6)
;
; save postscript version
;
      if setplot eq 'ps' then begin
         set_plot,'ps'
         xsize=nxdim/100.
         ysize=nydim/100.
         !psym=0
         !p.font=0
         device,font_size=9
         device,/landscape,bits=8,filename='monthly_mean_waccmsd_yz_'+yyyymm+'.ps'
         device,/color
         device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
                xsize=xsize,ysize=ysize
         !p.thick=2.0                   ;Plotted lines twice as thick
         !p.charsize=2.0
      endif
erase
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
nlvls=21L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
contour,u2zmean,alat,g2zmean,levels=-100.+10.*findgen(21),/noeras,xrange=[-90.,90],yrange=[10.,100.],title=yyyymm,charsize=2,c_color=col1,/fill,xtitle='Latitude',xticks=6,ytitle='Geopotential Height (km)',color=0
contour,u2zmean,alat,g2zmean,levels=-100.+10.*findgen(10),/noeras,/overplot,/follow,color=mcolor,c_linestyle=5
contour,u2zmean,alat,g2zmean,levels=10.+10.*findgen(10),/noeras,/overplot,/follow,color=0
contour,mark2zmean,alat,g2zmean,levels=[0.1,0.5,0.9],/noeras,/overplot,/follow,color=0,thick=5
tzm=0.*p2zmean
for k=0L,nth-1L do tzm(*,k)=th(k)*(p2zmean(*,k)/1000.)^.286
thzm=tzm*(1000./p2zmean)^0.286
contour,tzm,alat,g2zmean,levels=[150.],/follow,thick=3,/overplot,color=0.1*mcolor
contour,tzm,alat,g2zmean,levels=[160.],/follow,thick=3,/overplot,color=0.15*mcolor
contour,tzm,alat,g2zmean,levels=[170.],/follow,thick=3,/overplot,color=0.2*mcolor
contour,tzm,alat,g2zmean,levels=[180.],/follow,thick=3,/overplot,color=0.3*mcolor
contour,tzm,alat,g2zmean,levels=[190.],/follow,thick=3,/overplot,color=0.35*mcolor
contour,tzm,alat,g2zmean,levels=[200.],/follow,thick=3,/overplot,color=0.4*mcolor
contour,tzm,alat,g2zmean,levels=[210.],/follow,thick=3,/overplot,color=0.5*mcolor
contour,tzm,alat,g2zmean,levels=[220.],/follow,thick=3,/overplot,color=0.6*mcolor
contour,tzm,alat,g2zmean,levels=[230.],/follow,thick=3,/overplot,color=0.65*mcolor
contour,tzm,alat,g2zmean,levels=[240.],/follow,thick=3,/overplot,color=0.7*mcolor
contour,tzm,alat,g2zmean,levels=[250.],/follow,thick=3,/overplot,color=0.75*mcolor
contour,tzm,alat,g2zmean,levels=[260.],/follow,thick=3,/overplot,color=0.8*mcolor
contour,tzm,alat,g2zmean,levels=[270.],/follow,thick=3,/overplot,color=0.9*mcolor
contour,tzm,alat,g2zmean,levels=[280.],/follow,thick=3,/overplot,color=0.95*mcolor
contour,tzm,alat,g2zmean,levels=[290.],/follow,thick=3,/overplot,color=mcolor
contour,thzm,alat,gzm,levels=[1000.,2000.,3000.,4000.,5000.,6000.],/follow,thick=3,/overplot,c_annotation=strcompress([1000,2000,3000,4000,5000,6000],/remove_all)

omin=-100.
omax=100.
set_viewport,xmn,max(xorig)+xlen,ymn-0.12,ymn-0.12+0.01
!type=2^2+2^3+2^6
plot,[omin,omax],[0,0],yrange=[0,10],xrange=[omin,omax],xtitle='SD-WACCM Zonal Mean Wind (m/s)',/noeras,xstyle=1,color=0,charsize=2,charthick=2
ybox=[0,10,10,0,0]
x1=omin
dx=(omax-omin)/float(nlvls)
for j=0,nlvls-1 do begin
    xbox=[x1,x1,x1+dx,x1+dx,x1]
    polyfill,xbox,ybox,color=col1(j)
    x1=x1+dx
endfor

ofile=dirw+'f_1975-2010_2deg_refc1sd_wa4_tsmlt.002.cam.h5.'+yyyymm+'.nc3'
write_sdwaccm4_nc3,ofile,nc,nr,nth,alon,alat,th,pv2mean,p2mean,g2mean,u2mean,v2mean,q2mean,qdf2mean,mark2mean,sf2mean,h2o2mean,n2o2mean,o32mean

if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device,/close
   spawn,'convert -trim monthly_mean_waccmsd_yz_'+yyyymm+'.ps -rotate -90 monthly_mean_waccmsd_yz_'+yyyymm+'.jpg'
endif


endfor	; loop over months
end
