;
; for Laura and Matthias
; compute QDF and compute vortex marker field from WACCM data that is already on theta and
; has streamfunction (psi), PV, and zeta (relative vorticity)
; store .nc3 files with qdf and marker field
;
loadct,39
device,decompose=0
mcolor=byte(!p.color)
nlvls=20L
col1=1+(indgen(nlvls)/float(nlvls))*mcolor
PI2=6.2831853071796
DTR=PI2/360.
RADEA=6.37E6
setplot='ps'
read,'setplot=',setplot
nxdim=750
nydim=750
xorig=[0.1]
yorig=[0.15]
xlen=0.7
ylen=0.7
cbaryoff=0.02
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
;
; read dmps_0009-12-27.nc
;
idir='/Users/harvey/Brakebusch/Datfiles/'
;
; restore Laura's marker field
;
; AREA            FLOAT     = Array[20, 42]
; LAT             FLOAT     = Array[96]
; LON             FLOAT     = Array[144]
; MARKGRD         INT       = Array[20, 144, 96, 42]
; THETA           FLOAT     = Array[42]
;
ifile=idir+'laura_mark_0009-12-27.sav'
restore,ifile
markgrd_laura=markgrd
;
; restore my marker field
;
; ALAT            FLOAT     = Array[96]
; ALON            FLOAT     = Array[144]
; MARKGRD         FLOAT     = Array[144, 96, 42, 20]
; THLEV           FLOAT     = Array[42]
; TIME            FLOAT     = Array[20]
; 
ifile2=idir+'dmps_0009-12-27.sav'
restore,ifile2
markgrd_lynn=markgrd
nt=n_elements(time)
nth=n_elements(thlev)
;
; loop over time
;
!type=2^2+2^3
for itime=0,nt-1L do begin
    stime=strcompress(long(time(itime)),/remove_all)
    if setplot eq 'ps' then begin
       lc=0
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !p.font=0
       device,font_size=9
       device,/landscape,bits=8,/helvetica,/color,filename='step2_polar_mark_comparison_'+stime+'.ps'
       device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
              xsize=xsize,ysize=ysize
   !p.charsize=1.25
   !p.thick=2
   !p.charthick=5
   !p.charthick=5
   !y.thick=2
   !x.thick=2
    endif
;
; LOOP OVER THETA LEVELS
;
    erase
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
    map_set,90,0,0,/contin,/ortho,/noeras,/grid
    for ilev=35,0,-1L do begin	; ignore levels below 450 K
        pp=markgrd_laura(itime,*,*,ilev)
        contour,pp,alon,alat,levels=[0.1],/follow,thick=5,/overplot,color=(thlev(ilev)/(max(thlev)+100.))*mcolor,c_linestyle=5
        pp=markgrd_lynn(*,*,ilev,itime)
        contour,pp,alon,alat,levels=[0.1],/follow,thick=5,/overplot,color=(thlev(ilev)/(max(thlev)+100.))*mcolor
    endfor	; loop over theta
imin=thlev(35)
imax=max(thlev)
ymnb=ymn -cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn,xmx,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],/noeras,$
      xtitle='Theta',color=0,charsize=1.25,charthick=1.5
ybox=[0,10,10,0,0]
x2=imin
dx=(imax-imin)/(float(n_elements(col1)))
for jj=0L,n_elements(col1)-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(jj)
    x2=x2+dx
endfor

    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim step2_polar_mark_comparison_'+stime+'.ps -rotate -90 step2_polar_mark_comparison_'+stime+'.jpg'
    endif
endfor	; loop over days
end
