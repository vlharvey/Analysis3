;
; read CIPS level 3 data and plot average daisy for time range. NH. DFS=June 21
;
; netcdf cips_sci_3a_2008-322_v04.20_r04 {
; dimensions:
;         string = 17 ;
;         dim1_DEPENDENT2AVERSION = 15 ;
;         dim1_PETAL_START_TIME = 15 ;
;         dim1_BBOX = 4 ;
;         dim1_ORBIT_NUMBERS = 15 ;
;         dim1_ALBEDO = 1953 ;
;         dim2_ALBEDO = 1953 ;
; variables:
;         int UT_DATE ;
;         char VERSION(string) ;
;         char PRODUCT_CREATION_TIME(string) ;
;         byte DEPENDENT2AVERSION(dim1_DEPENDENT2AVERSION) ;
;         char HEMISPHERE(string) ;
;         float CENTER_LONGITUDE ;
;         double PETAL_START_TIME(dim1_PETAL_START_TIME) ;
;         float FIRST_IMAGE_START ;
;         float KM_PER_PIXEL ;
;         int BBOX(dim1_BBOX) ;
;         short ORBIT_NUMBERS(dim1_ORBIT_NUMBERS) ;
;         int QUALITY_FLAGS ;
;         float ALBEDO(dim2_ALBEDO, dim1_ALBEDO) ;
;         char DELTA_FLAT_FILE(string) ;
;         float DELTA_FLAT_NORMALIZATION_PX ;
;         float DELTA_FLAT_NORMALIZATION_PY ;
;         float DELTA_FLAT_NORMALIZATION_MX ;
;         float DELTA_FLAT_NORMALIZATION_MY ;
;
loadct,39
device,decompose=0
mcolor=byte(!p.color)
!NOERAS=-1
SETPLOT='ps'
read,'setplot',setplot
nxdim=750
nydim=750
xorig=[0.15]
yorig=[0.15]
xlen=0.7
ylen=0.7
cbaryoff=0.03
cbarydel=0.02
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,.5*cos(a),.5*sin(a),/fill
nlvls=81
col1=1+indgen(nlvls)*mcolor/nlvls
;
; restore CIPS procedures and functions
;
restore,'read_cips_file.sav
pth='/Volumes/Data/CIPS_data/Datfiles/cips_sci_3a_'
;
; LOWER LIMIT FOR ALBEDO -- ANYTHING SMALLER THAN THIS IS ASSUMED NOT TO BE A CLOUD.
; 
LIM=0.

dfs0=-10.
dfs1=30.
iyear=2007
;
; Ask interactive questions- get starting/ending date (DFS) and YYYY
;
;print, ' '
;read,' Enter starting DFS ',dfs0
;read,' Enter ending DFS ',dfs1
read,' Enter YYYY ',iyear
;
; convert DFS to DOY
;
z=kgmt(6,21,iyear,soldoy)
iday0=soldoy+dfs0
iday1=soldoy+dfs1
nday=long(iday1-iday0+1.)
icount=0L
syear=string(FORMAT='(I4)',iyear)
;goto,jump2plot
;
; loop over days
;
for iday=iday0,iday1 do begin
    sday=string(FORMAT='(I3.3)',iday)
;
; read CIPS L3 nc file on this day
;
    spawn,'ls '+pth+syear+'-'+sday+'_v04.20_r04.nc',fname
    if fname(0) eq '' then stop,'missing '+syear+'-'+sday
    data = read_cips_file(fname)
;
; extract variables from data structure
;
    alat=data.latitude
    alon=data.longitude
    ut_date=data.ut_date
    hemisphere=data.hemisphere
    albedo=data.albedo
    result=size(alat)
    nc=result(1)
    nr=result(2)
;
; check
;
;erase
;!type=2^2+2^3
;xmn=xorig(0)
;xmx=xorig(0)+xlen
;ymn=yorig(0)
;ymx=yorig(0)+ylen
;set_viewport,xmn,xmx,ymn,ymx
;; LIMIT has the form [Latmin, Lonmin, Latmax, Lonmax] 
;map_set,90,0,0,/ortho,/contin,/grid,color=0,/noeras,title='CIPS L3 V4.20 on '+strcompress(ut_date),limit=[50, 0, 90, 360]
;level=[.1,1.+findgen(nlvls-1)]
;bad=where(finite(albedo) eq 0 or albedo eq 0.)
;if bad(0) ne -1L then albedo(bad)=0./0.
;good=where(finite(albedo) eq 1)
;if good(0) ne -1L then print,ut_date,' Max Albedo = ',max(albedo(good))
;contour,albedo,alon,alat,/fill,levels=level,/noeras,/overplot,c_color=col1
;imin=min(level)
;imax=max(level)
;ymnb=ymn -cbaryoff
;ymxb=ymnb+cbarydel
;set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
;!type=2^2+2^3+2^6
;index=where(level ge 1.)
;slevel=['0.1',strcompress(long(level(index)),/remove_all)]
;xtickindex=where(level mod 10 eq 0,nxticks)
;plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,xticks=nxticks,xtickname=['1',slevel(xtickindex)],$
;      xtitle='Albedo (x10-6 sr-1)'
;ybox=[0,10,10,0,0]
;x2=imin
;dx=(imax-imin)/(float(nlvls)-1)
;for j=1,nlvls-1 do begin
;    xbox=[x2,x2,x2+dx,x2+dx,x2]
;    polyfill,xbox,ybox,color=col1(j)
;    x2=x2+dx
;endfor
;
    BAD=WHERE(FINITE(ALBEDO) EQ 0 OR ALBEDO LE LIM,nbad)
    IF NBAD GT 0 THEN ALBEDO(BAD)=-99.
;
; declare array to store averages
;
    if icount eq 0L then begin
       ALB_avg=fltarr(nc,nr)
       NALB_avg=fltarr(nc,nr)
    endif
    if icount ge 0L then begin
       good=where(albedo ne -99.)
       if good(0) ne -1L then begin
          ALB_avg(good)=ALB_avg(good)+albedo(good)
          NALB_avg(good)=NALB_avg(good)+1.
       endif
    endif
    icount=icount+1
;
ENDFOR   ; loop over days
;
; average Daisy
;
index=where(nalb_avg ne 0.)
if index(0) ne -1L then ALB_avg(index)=ALB_avg(index)/nalb_avg(index)
;
; save time averaged daisy file
;
ofile=pth+syear+'_v04.20_nh.sav'
print,ofile
SAVE,file=ofile,alat,alon,alb_avg,syear
;
; plot averaged Daisy
;
jump2plot:
;ofile=pth+syear+'_v04.20_nh.sav'
;restore,ofile
if setplot eq 'ps' then begin
   xsize=nxdim/100.
   ysize=nydim/100.
   set_plot,'ps'
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_avg_daisy_nh_'+syear+'.ps'
   !p.charsize=1.5
   !p.thick=1.5
   !p.charthick=5
   !p.charthick=5
   !y.thick=1.5
   !x.thick=1.5
endif
erase
!type=2^2+2^3
nlvls=41
col1=1+indgen(nlvls)*mcolor/nlvls
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
; LIMIT has the form [Latmin, Lonmin, Latmax, Lonmax]
map_set,90,0,0,/ortho,/contin,/grid,color=0,/noeras,title='CIPS L3 V4.20 on -10 to +30 DFS in '+syear,limit=[50, 0, 90, 360]
level=[.1,1.+findgen(nlvls-1)]
bad=where(alb_avg le 0.)
if bad(0) ne -1L then alb_avg(bad)=0./0.
contour,alb_avg,alon,alat,/fill,levels=level,/noeras,/overplot,c_color=col1
contour,smooth(alb_avg,9,/Nan),alon,alat,/follow,levels=[5.,10.,20.,30.],/noeras,/overplot,color=mcolor,c_labels=[1,1,1,1]
imin=min(level)
imax=max(level)
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn+0.01,xmx-0.01,ymnb,ymxb
!type=2^2+2^3+2^6
index=where(level ge 1.)
slevel=['0.1',strcompress(long(level(index)),/remove_all)]
xtickindex=where(level mod 10 eq 0,nxticks)
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,color=0,xticks=nxticks,xtickname=['1',slevel(xtickindex)],$
      xtitle='Albedo (x10-6 sr-1)'
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(nlvls)-1)
for j=1,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor
if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device, /close
   spawn,'convert -trim polar_avg_daisy_nh_'+syear+'.ps -rotate -90 polar_avg_daisy_nh_'+syear+'.jpg'
;  spawn,'rm -f polar_avg_daisy_nh_'+syear+'.ps'
endif
end
