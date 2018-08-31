;
; plot mercator projection of seasonal average average albedo
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
z=kgmt(12,21,iyear,soldoy)
iday0=soldoy+dfs0
iday1=soldoy+dfs1
nday=long(iday1-iday0+1.)
icount=0L
syear=string(FORMAT='(I4)',iyear)
;
; plot averaged Daisy
;
jump2plot:
ofile=pth+syear+'_v04.20_sh.sav'
restore,ofile
if setplot eq 'ps' then begin
   xsize=nxdim/100.
   ysize=nydim/100.
   set_plot,'ps'
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='merc_avg_daisy_sh_'+syear+'.ps'
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
map_set,0,0,0,/contin,/grid,color=0,/noeras,title='CIPS L3 V4.20 on -10 to +30 DFS in '+syear,limit=[-80, 0, -50, 360]
level=[.1,1.+findgen(nlvls-1)]
bad=where(finite(alb_avg) ne 1 or alb_avg le 0.)
;if bad(0) ne -1L then alb_avg(bad)=0./0.
contour,alb_avg,alon,alat,/fill,levels=level,/noeras,/overplot,c_color=col1
contour,smooth(alb_avg,31,/Nan),alon,alat,/follow,levels=[5.,10.,20.,30.],/noeras,/overplot,color=mcolor,c_labels=[1,1,1,1],thick=5
map_set,0,0,0,/contin,/grid,color=0,/noeras,limit=[-80, 0, -50, 360]
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
   spawn,'convert -trim merc_avg_daisy_sh_'+syear+'.ps -rotate -90 merc_avg_daisy_sh_'+syear+'.jpg'
;  spawn,'rm -f merc_avg_daisy_sh_'+syear+'.ps'
endif
end
