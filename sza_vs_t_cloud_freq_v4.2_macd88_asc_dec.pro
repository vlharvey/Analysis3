;
; plot SZA-time cloud frequency. User enters Quality Flag and Albedo values.
; QF=0, 1, 2
; Albedo=0, 1, 2, ... 10
; SZA range: 42-94
; 2 panel, 1 for asc and 1 for dec 
;
loadct,39
mcolor=!p.color
icolmax=byte(!p.color)
mcolor=icolmax
icmm1=icolmax-1B
icmm2=icolmax-2B
device,decompose=0
!NOERAS=-1
SETPLOT='ps'
read,'setplot',setplot
nxdim=750
nydim=750
xorig=[0.20,0.20]
yorig=[0.60,0.18]
xlen=0.6
ylen=0.275
cbaryoff=0.04
cbarydel=0.02
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
a=findgen(8)*(2*!pi/8.)
usersym,1.2*cos(a),1.2*sin(a),/fill
nr=181L
latbin=-90.+findgen(nr)
nsza=221
szabin=40.+0.25*findgen(nsza)	; 40 to 95 degrees
solday=172L	; doy of NH summer solstice

;pth='/aura7/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on aura
;pth='/Users/harvey/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on macp98
pth='/Volumes/data/CIPS_data/Datfiles/cips_sci_2_orbit_'	; run on macd88

lstmn=9
lstdy=1
lstyr=2009
ledmn=9
leddy=20
ledyr=2009
lstday=0
ledday=0
;
; Ask interactive questions- get starting/ending date
;
;print, ' '
;read,' Enter starting date (month, day, year) ',lstmn,lstdy,lstyr
;read,' Enter ending date   (month, day, year) ',ledmn,leddy,ledyr
if lstyr lt 91 then lstyr=lstyr+2000
if ledyr lt 91 then ledyr=ledyr+2000
if lstyr lt 1900 then lstyr=lstyr+1900
if ledyr lt 1900 then ledyr=ledyr+1900
if lstyr lt 1991 then stop,'Year out of range '
if ledyr lt 1991 then stop,'Year out of range '
z = stddat(lstmn,lstdy,lstyr,lstday)
z = stddat(ledmn,leddy,ledyr,ledday)
if ledday lt lstday then stop,' Wrong dates! '
kday=ledday-lstday+1L
asc_freq=fltarr(kday,nsza)
des_freq=fltarr(kday,nsza)
doy=fltarr(kday)
dfs=fltarr(kday)
icount=0
;goto,plotit
;
; Compute initial Julian date
;
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
;
; loop over days
;
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr

; --- Test for end condition and close windows.
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then goto,plotit
      syear=string(FORMAT='(I4)',iyr)
      smn=string(FORMAT='(I2.2)',imn)
      sdy=string(FORMAT='(I2.2)',idy)
      sday=string(FORMAT='(I3.3)',iday)
      sdate=syear+smn+sdy
      print,sdate,' ',sday
      doy(icount)=iday
      dfs(icount)=iday-solday
      ofile=pth+sdate+'_all.sav
      dum=findfile(ofile)
      if dum(0) eq '' then goto,skipday
;
; read data
; latitude_orig_all,latitude_all,longitude_all,ut_time_all,SZA_all,CLOUD_INDEX_all,ALB_all,ALB_ERR_all,$
; RAD_all,RAD_ERR_all,IWC_all,IWC_ERR_all,ICD_all,QUALITY_FLAGS_all,NLAYERS_all
;
      restore,ofile
;
; ascending and descending cloud frequencies as a function of SZA
;
for j=0L,nsza-1L do begin
    ymin=szabin(j)-0.125
    ymax=szabin(j)+0.125
    index=where(SZA_all ge ymin and SZA_all le ymax and quality_flags_all eq 0 and alb_all gt 0. and (latitude_orig_all gt 90. or latitude_orig_all lt -90.),nasc)
    all=where(SZA_all ge ymin and SZA_all le ymax and (latitude_orig_all gt 90. or latitude_orig_all lt -90.),nascall)
    if index(0) ne -1L then asc_freq(icount,j)=100.*float(nasc)/float(nascall)
if asc_freq(icount,j) gt 1. then print,'ASC '+sdate+' ',szabin(j),min(sza_all(index)),max(sza_all(index)),min(alb_all(index)),max(alb_all(index)) 
    index=where(SZA_all ge ymin and SZA_all le ymax and quality_flags_all eq 0 and alb_all gt 0. and latitude_orig_all le 90. and latitude_orig_all ge -90.,ndes)
    all=where(SZA_all ge ymin and SZA_all le ymax and latitude_orig_all le 90. and latitude_orig_all ge -90.,ndesall)
    if index(0) ne -1L then des_freq(icount,j)=100.*float(ndes)/float(ndesall)
if des_freq(icount,j) gt 1. then print,'DES '+sdate+' ',szabin(j),min(sza_all(index)),max(sza_all(index)),min(alb_all(index)),max(alb_all(index))
endfor

skipday:
icount=icount+1L
goto,jump
;
; plot
;
plotit:
;restore,'sza_vs_t_20090901-20090920_QF0_ALB0.sav'	;,asc_freq,des_freq,dfs,szabin,syear
if setplot eq 'ps' then begin
   xsize=nxdim/100.
   ysize=nydim/100.
   set_plot,'ps'
   !p.font=0
   device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
          /bold,/color,bits_per_pixel=8,/helvetica,filename='sza_vs_t_cloud_freq_v4.2_'+syear+'.ps'
   !p.charsize=1.5
   !p.thick=1.5
   !p.charthick=5
   !p.charthick=5
   !y.thick=1.5
   !x.thick=1.5
endif
erase
xyouts,.15,.95,'CIPS Version 4.20 Cloud Frequency',/normal,color=0,charsize=2
level=[0.001,0.01,0.05,0.1,0.5,0.6,0.7,0.8,0.9,1.,5,10,15]
nlvls=n_elements(level)
col1=1+indgen(nlvls)*mcolor/nlvls
!type=2^2+2^3
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,asc_freq,dfs,szabin,c_color=col1,/fill,xtitle='DFS '+syear,ytitle='SZA',title='Ascending Node',xrange=[min(dfs),max(dfs)],yrange=[40,95],/noeras,levels=level,color=0
contour,asc_freq,dfs,szabin,color=0,/follow,/overplot,/noeras,levels=level,c_labels=0*indgen(nlvls)
xmn=xorig(1)
xmx=xorig(1)+xlen
ymn=yorig(1)
ymx=yorig(1)+ylen
set_viewport,xmn,xmx,ymn,ymx
contour,des_freq,dfs,szabin,c_color=col1,/fill,xtitle='DFS '+syear,ytitle='SZA',title='Descending Node',xrange=[min(dfs),max(dfs)],yrange=[40,95],/noeras,levels=level,color=0
contour,des_freq,dfs,szabin,color=0,/follow,/overplot,/noeras,levels=level,c_labels=0*indgen(nlvls)
xyouts,.2,.05,'QF=0 and ALB GT 0',/normal,color=0,charsize=1.5
;
; color bar
;
!type=2^2+2^3+2^5
xmnb=xorig(0)+xlen+cbaryoff
xmxb=xmnb+cbarydel
ymnb=min(yorig)+0.1
ymxb=max(yorig)+ylen-0.1
set_viewport,xmnb,xmxb,ymnb,ymxb
slab=' '+strarr(n_elements(level))
plot,[0,0],[min(level),max(level)],xrange=[0,10],color=0,$
     yticks=n_elements(level)-1L,ytickname=slab,$
     yrange=[min(level),max(level)],charsize=2,title='(%)',charthick=2
xbox=[0,10,10,0,0]
y1=min(level)
dy=(max(level)-min(level))/float(nlvls)
for j=0,nlvls-1 do begin
    ybox=[y1,y1,y1+dy,y1+dy,y1]
    polyfill,xbox,ybox,color=col1(j)
    y1=y1+dy
endfor
slab=strcompress(string(format='(f6.3)',level),/remove_all)
y1=ymnb	;min(level)+dy/2
dy=(ymxb-ymnb)/float(nlvls)
for i=0L,n_elements(slab)-1L do begin
    slab0=slab(i)
    flab0=float(slab(i))
    if flab0 lt 0.01 then begin
       slab0=strcompress(string(format='(f5.3)',flab0),/remove_all)
       xyouts,xorig(0)+xlen+0.07,y1,slab0,/normal,color=0
    endif
    if flab0 lt 1. and flab0 ge 0.01 then begin
       slab0=strcompress(string(format='(f4.2)',flab0),/remove_all)
       xyouts,xorig(0)+xlen+0.07,y1,slab0,/normal,color=0
    endif
    if flab0 ge 1. then begin
       slab0=strcompress(long(slab0),/remove_all)
       xyouts,xorig(0)+xlen+0.07,y1,slab0,/normal,color=0
    endif
    y1=y1+dy
endfor

;
; Close PostScript file and return control to X-windows
 if setplot ne 'ps' then stop
 if setplot eq 'ps' then begin
    device, /close
    spawn,'convert -trim sza_vs_t_cloud_freq_v4.2_'+syear+'.ps -rotate -90 '+$
                        'sza_vs_t_cloud_freq_v4.2_'+syear+'.jpg'
;   spawn,'rm -f sza_vs_t_cloud_freq_v4.2_'+syear+'.ps'
 endif
save,file='sza_vs_t_20090901-20090920_QF0_ALB0.sav',asc_freq,des_freq,dfs,szabin,syear
end
