; 
; plot ATTREX trajectories
; 
@rd_geos5_nc3_meto

loadct,39
device,decompose=0
mcolor=255
!p.background=mcolor
!p.color=0
icmm1=mcolor-1B
icmm2=mcolor-2B
nxdim=750
nydim=750
xorig=[0.1]
yorig=[0.25]
xlen=0.8
ylen=0.6
cbaryoff=0.05
cbarydel=0.01
nlvls=21
col1=1.+indgen(nlvls)*mcolor/nlvls
!noeras=1
a=findgen(8)*(2*!pi/8.)
usersym,.5*cos(a),.5*sin(a),/fill
nh=1
ifilet='../'+[$
;'geos5_20130205_attrex_10d_back.traj',$
;'geos5_20130209_attrex_10d_back.traj',$
;'geos5_20130214_attrex_10d_back.traj',$
;'geos5_20130221_attrex_10d_back.traj',$
;'geos5_20130226_attrex_10d_back.traj',$
'geos5_20130301_attrex_10d_back.traj'$
]
nruns=n_elements(ifilet)
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
     'jul_','aug_','sep_','oct_','nov_','dec_']
!NOERAS=-1
setplot='ps'
read,'enter setplot',setplot
if setplot ne 'ps' then $
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
!p.background=mcolor
dir='/Volumes/earth/harvey/GEOS5_data/Datfiles/'
for nn=0L,nruns-1L do begin
;
;
; restore trajectory locations in IDL SAVE format
;
    restore,ifilet(nn)+'.sav'	;,xsave,ysave,zsave,pvsave,psave,tmpsave,gphsave,agesave,mdates,etimes
print,'restored ',ifilet(nn)+'.sav'

    if setplot eq 'ps' then begin
       set_plot,'ps'
       xsize=nxdim/100.
       ysize=nydim/100.
       !psym=0
       !p.font=0
       !p.charthick=2
       device,font_size=9
       device,/color,/landscape,bits=8,filename=$
              'trajectories_attrex_'+strcompress(mdates(0),/remove_all)+'.ps'
       device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
              xsize=xsize,ysize=ysize
    endif
;
;     for n=0,nfiles-1 do begin
n=0
         stime0=strcompress(mdates(0),/remove_all)
         stime1=strcompress(mdates(n),/remove_all)
         setime=strcompress(long(abs(etimes(n))),/remove_all)       ; elapsed hours
         syr=strmid(stime1,0,4)
         smn=strmid(stime1,4,2)
         sdy=strmid(stime1,6,2)
;
; read GEOS data
;
        ncfile0=dir+'DAS.ops.asm.tavg3d_dyn_v.GEOS520.MetO.'+stime0+'_AVG.V01.nc3'
        rd_geos5_nc3_meto,ncfile0,nc,nr,inth,alon,alat,th,$
                  pv2,p2,msf2,u2,v2,q2,qdf2,mark2,sf2,vp2,iflag
    erase
;
; temperature and height
;
    tmp2=0.*pv2
    for k=0L,inth-1L do tmp2(*,*,k)=th(k)*(p2(*,*,k)/1000.)^.286
;   gph2=gph2/1000.

    klev=where(th eq 500.)
    k=klev(0)
    tmp1=fltarr(nc+1,nr)
    x=fltarr(nc+1)
    x(0:nc-1)=alon
    x(nc)=x(0)+360.
    tmp1(0:nc-1,0:nr-1)=transpose(msf2(*,*,k))
    tmp1(nc,*)=tmp1(0,*)
    !type=2^2+2^3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
;   MAP_SET,0,0,0,/GRID,/contin,charsize=2,/noeras,color=0,title='ATTREX '+stime0,/hires,/usa,limit=[min(ysave)-5.,0.,max(ysave)+5.,360.]
    MAP_SET,0,0,0,/GRID,/contin,charsize=2,/noeras,color=0,title='ATTREX '+stime0,/hires,/usa,limit=[-30.,0.,60.,360.]
;   print,th(k),min(tmp2(*,*,k)),max(tmp2(*,*,k)),min(gph2(*,*,k)),max(gph2(*,*,k))
;loadct,0
;   contour,tmp1,x,alat,/overplot,/cell_fill,c_color=col1,levels=min(tmp1(*,0:nr/2))+ ((max(tmp1(*,0:nr/2))-min(tmp1(*,0:nr/2)))/float(nlvls))*findgen(nlvls)
    contour,tmp1,x,alat,/overplot,/follow,color=0,levels=long( min(tmp1)+ ((max(tmp1)-min(tmp1))/float(nlvls))*findgen(nlvls) ),c_labels=0*intarr(nlvls),thick=2
;    contour,tmp1,x,alat,/overplot,/follow,color=mcolor,levels=[130],thick=15
;loadct,39
;   contour,transpose(gph2(*,*,k)),alon,alat,/overplot,/follow,color=mcolor,levels=min(gph2(*,*,k))+ ((max(gph2(*,*,k))-min(gph2(*,*,k)))/float(nlvls))*findgen(nlvls),c_labels=1+0*intarr(nlvls),c_charsize=3
;   map_set,0,0,0,/contin,/grid,color=mcolor,/noeras
    xx0=reform(xsave(0,*))
    yy0=reform(ysave(0,*))
    aa0=reform(agesave(0,*))
    tmp0=reform(tmpsave(0,*))
    coinindex=where(abs(tmp0) ge 100.,ncoin)
    oplot,xx0(coinindex),yy0(coinindex),psym=8,color=0
nfiles=n_elements(mdates)
    for nn=1,nfiles-2 do begin
;       print,mdates(nn)
        xx0=reform(xsave(nn,*))
        yy0=reform(ysave(nn,*))
        aa0=reform(agesave(nn,*))
        xx1=reform(xsave(nn+1,*))
        yy1=reform(ysave(nn+1,*))
        aa1=reform(agesave(nn+1,*))
;oplot,xx0(coinindex),yy0(coinindex),color=(aa0(coinindex(0))/max(abs(agesave)))*mcolor
        for icoin=0L,ncoin-1,100 do begin
;;print,icoin,yy0(coinindex(icoin)),yy1(coinindex(icoin))
             oplot,[xx0(coinindex(icoin)),xx1(coinindex(icoin))],[yy0(coinindex(icoin)),yy1(coinindex(icoin))],psym=8,color=(abs(aa0(coinindex(icoin)))/max(abs(agesave)))*mcolor
;            oplot,[xx0(coinindex(icoin)),xx0(coinindex(icoin))],[yy0(coinindex(icoin)),yy0(coinindex(icoin))],psym=8,color=(abs(aa0(coinindex(icoin)))/abs(min(agesave)))*mcolor
;            oplot,[xx1(coinindex(icoin)),xx1(coinindex(icoin))],[yy1(coinindex(icoin)),yy1(coinindex(icoin))],psym=8,color=(abs(aa0(coinindex(icoin)))/abs(min(agesave)))*mcolor
        endfor
    endfor
;   contour,tmp1,x,alat,/overplot,/follow,color=mcolor,levels=[130],thick=15,c_linestyle=2,c_charthick=5,c_charsize=2
    xx0=reform(xsave(0,*))
    yy0=reform(ysave(0,*))
    oplot,xx0(coinindex),yy0(coinindex),psym=8,color=0
;
; color bar
;
kmin=0
kmax=max(abs(agesave))
ymnb=ymn -.5*cbaryoff
ymxb=ymnb+cbarydel
set_viewport,xmn,xmx,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[kmin,kmax],[0,0],yrange=[0,10],xrange=[kmin,kmax],/noeras,color=0,xtitle='ATTREX Trajectory Age (hours)',charsize=2,charthick=2
ybox=[0,10,10,0,0]
x2=kmin
dx=(kmax-kmin)/float(nlvls)
for j=0,nlvls-1 do begin
    xbox=[x2,x2,x2+dx,x2+dx,x2]
    polyfill,xbox,ybox,color=col1(j)
    x2=x2+dx
endfor
if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
   device,/close
   spawn,'convert trajectories_attrex_'+strcompress(mdates(0),/remove_all)+'.ps'+$
         ' -rotate -90 trajectories_attrex_'+strcompress(mdates(0),/remove_all)+'.jpg'
   spawn,'rm -f trajectories_attrex_'+strcompress(mdates(0),/remove_all)+'.ps'
endif
endfor	; loop over flights
end
