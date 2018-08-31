;
; Read in Level 2a RAA filtered data and plot daily polar map
; VLH 8/28/18
;    
@stddat
@kgmt
@ckday
@kdate

loadct,0
a=findgen(8)*(2*!pi/8.)
usersym,1.5*cos(a),1.5*sin(a),/fill
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
device,decompose=0
!p.background=icolmax
setplot='ps'
read,'setplot=',setplot
nxdim=750
nydim=750
xorig=[0.1]
yorig=[0.2]
xlen=0.8
ylen=0.6
cbaryoff=0.06
cbarydel=0.01
!NOERAS=-1
if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif

RAA_Level2_path='/atmos/harvey/CIPS_data/Datfiles/RAA/'

lstmn=6L & lstdy=1L & lstyr=2016L
ledmn=6L & leddy=30L & ledyr=2016L
lstday=0L & ledday=0L
;read,' Enter starting date (month, day, year) ',lstmn,lstdy,lstyr
;read,' Enter ending date   (month, day, year) ',ledmn,leddy,ledyr
lstday=julday(lstmn,lstdy,lstyr)
ledday=julday(ledmn,leddy,ledyr)
if ledday lt lstday then stop,' Wrong dates! '

kday=ledday-lstday+1L
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
icount=0L
;
; --- Loop here --------
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr

; --- Test for end condition and close windows.
      ndays=julday(imn,idy,iyr)
      if ndays lt lstday then stop,' starting day outside range '
      if ndays gt ledday then stop,' normal termination condition '
      if iyr ge 2000 then iyr1=iyr-2000
      if iyr lt 2000 then iyr1=iyr-1900
      syr=string(FORMAT='(I4.4)',iyr)
      smn=string(FORMAT='(I2.2)',imn)
      sdy=string(FORMAT='(I2.2)',idy)
      sday=string(FORMAT='(I3.3)',iday)
      sdate=syr+smn+sdy
;
; find CIPS orbits for this day
;
      infile = file_search(RAA_Level2_path+'CIPS_RAA_Level3a_doy-'+sday+'_orbit-*.sav', count = count)
      if count lt 1L then goto,jump

      if setplot eq 'ps' then begin
         lc=0
         set_plot,'ps'
         xsize=nxdim/100.
         ysize=nydim/100.
         !p.font=0
         device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
                /bold,/color,bits_per_pixel=8,/helvetica,filename='polar_cips_raa_level2a_'+sdate+'.ps'
         !p.charsize=1.25
         !p.thick=2
         !p.charthick=5
         !y.thick=2
         !x.thick=2
      endif

      loadct,0
      erase
      !type=2^2+2^3
      xmn=xorig(0)
      xmx=xorig(0)+xlen
      ymn=yorig(0)
      ymx=yorig(0)+ylen
      set_viewport,xmn,xmx,ymn,ymx
      map_set,-90,0,90,/ortho,/contin,/grid,title=sdate,color=0,/noerase,limit=[-30.,0.,-90.,360.]
;     map_set,0,0,0,/MOLLWEIDE,/contin,/grid,title=sdate,color=0,/noerase,limit=[-30.,0.,-65.,360.]
;     map_set,0,0,0,/good,/contin,/grid,title=sdate,color=0,/noerase
;     map_set,0,0,0,/aitoff,/contin,/grid,title=sdate,color=0,/noerase
      MAP_CONTINENTS, fill_continents=1,color = 150
      loadct,70
      for iorbit = 0, count - 1L do begin ; loop over orbits
          print, infile(iorbit)
          restore,infile(iorbit)

;         if iorbit eq 0L then begin
             FILTERED_RAA_ALL=transpose(FILTERED_RAA)
             FILTERED_RAA_VARIANCE_ALL=transpose(FILTERED_RAA_VARIANCE)
             LATITUDE_ALL=transpose(LATITUDE)
             LONGITUDE_ALL=transpose(LONGITUDE)
;         endif 
;         if iorbit gt 0L then begin
;            FILTERED_RAA_ALL=[FILTERED_RAA_ALL,transpose(FILTERED_RAA)]
;            FILTERED_RAA_VARIANCE_ALL=[FILTERED_RAA_VARIANCE_ALL,transpose(FILTERED_RAA_VARIANCE)]
;            LATITUDE_ALL=[LATITUDE_ALL,transpose(LATITUDE)]
;            LONGITUDE_ALL=[LONGITUDE_ALL,transpose(LONGITUDE)]
;         endif
;     endfor
;
; sort scenes by scene-mean variance value
;
;result=mean(FILTERED_RAA_VARIANCE_ALL,dim=2,/Nan)
;var_sort=mean(result,dim=1,/Nan)
;index=sort(var_sort)
;FILTERED_RAA_ALL=FILTERED_RAA_ALL(index,*,*)
;LATITUDE_ALL=LATITUDE_ALL(index,*,*)
;LONGITUDE_ALL=LONGITUDE_ALL(index,*,*)
;
; loop over scenes
;
          result=size(LONGITUDE_ALL)
          nscenes=result(1)
          if iorbit eq 0L then begin
             nlvls=25L
             imin=-5.0	;min(FILTERED_RAA_ALL,/Nan)
             imax=5.0	;max(FILTERED_RAA_ALL,/Nan)
             level=imin+((imax-imin)/float(nlvls))*findgen(nlvls+1)
             nlvls=n_elements(level)
             col1=reverse(1+(findgen(nlvls)/float(nlvls))*255L)
          endif
          for ii=0L,nscenes-1L do begin
              raa=reform(FILTERED_RAA_ALL(ii,*,*))
              lat=reform(LATITUDE_ALL(ii,*,*))
              lon=reform(LONGITUDE_ALL(ii,*,*))
              contour,raa,lon,lat,levels=level,/cell_fill,c_color=col1,/overplot
           endfor	; loop over scenes

      endfor	; loop over orbits
loadct,0
lonlab = findgen(4)*90
latlab = findgen(6)*10.-80.
cips_lon=180.
cips_lat=-60.
for i = 0, n_elements(latlab) - 1L do xyouts, floor(cips_lon/15.)*15, latlab, string(latlab,format = '(i3.2)'),align = .5,color=0
;for i = 0, n_elements(lonlab) - 1L do xyouts, lonlab,floor(cips_lat/10.)*10,  string(lonlab,format = '(i4.3)'),align = .5,color=0

      !type=2^2+2^3+2^6                     ; no y title or ticsks
      imin=min(level)
      imax=max(level)
      slab=' '+strarr(nlvls/2L)
;     slab=string(format='(f4.1)',level(0:-1:2))
      !p.title = ' '
      plot,[0,1],[0,0],yrange=[0,10],xrange=[0,1],/noeras,xticks=nlvls/2L-1,charsize=1.25,$
           position = [xmn,ymn-0.07,xmx,ymn-0.01],xstyle=1,xtickname=slab,xtitle= 'CIPS Filtered RAA (%)',color=0,charthick=2
      ybox=[0,10,10,0,0]
      x2=0
      loadct,70
      for j=1,n_elements(col1)-1 do begin
          dx= 1./float(nlvls-1)
          xbox=[x2,x2,x2+dx,x2+dx,x2]
          polyfill,xbox,ybox,color=col1[j-1]
          x2=x2+dx
      endfor
      loadct,0
      dx=1/(float(nlvls/2.))
      x1=dx/2
      slab=string(format='(f4.1)',level(0:-1:2))
      slabcolor=fltarr(n_elements(slab))*0
      slabcolor(0:2)=255
      for i=0L,n_elements(slab)-1L do begin
          xyouts,x1-dx/2.,5,slab[i],charsize=1.5,charthick=2,/data,color=slabcolor[i], orientation= -90.,align = .5
          x1=x1+dx
      endfor

      if setplot ne 'ps' then stop
      if setplot eq 'ps' then begin
         device, /close
         spawn,'convert -trim polar_cips_raa_level2a_'+sdate+'.ps -rotate -90 polar_cips_raa_level2a_'+sdate+'.png'
;        spawn,'rm -f polar_cips_raa_level2a_'+sdate+'.ps'
      endif

goto,jump
end
