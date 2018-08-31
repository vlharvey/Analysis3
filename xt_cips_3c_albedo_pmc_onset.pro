;
; Lat=60, 70, 80
; NH May-Jun; SH Nov-Dec
; plot Hovmollers of CIPS Albedo. Use 2G files

;FOR FREQUENCY, WE ONLY NEED THE "ALL" FILES.

;THIS CODE WORKS WITH THE 1-DEGREE LATITUDE BIN FILES.
;LATITUDES GO FROM 50-51 TO 84-85 AND 85-84 TO 51-50, IN 1-DEGREE INCREMENTS.

loadct,39
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
xorig=[0.25]
yorig=[0.25]
xlen=0.5
ylen=0.7
cbaryoff=0.12
cbarydel=0.01
!NOERAS=-1

pth='/atmos/harvey/CIPS_data/Datfiles/Level_3c_Summary/'
ptho='/Volumes/Data/CIPS_data/Pre_process/'

nlon=13L
lonbin=30.*findgen(nlon)
dx=lonbin(1)-lonbin(0)

G = '2'
;read,'Input the threshold (1,2,5) >> ',G

slat='60'
read,'Input the latitude (positive) >> ',slat

if setplot ne 'ps' then window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162

!p.charsize=1.8
!P.FONT=1
!P.THICK=3
!p.multi=0

!Y.THICK=3
!y.style=1
;!Y.MARGIN=3

!X.THICK=3
!x.style=1
!y.range=[80,-40]
;!x.range=[20,33]
!y.title='Days From Solstice'
;!X.MARGIN=4

seasons=['NH07','NH08','NH09','NH10','NH11','SH07','SH08','SH09','SH10']
seasons=['NH07','NH08','NH09','NH10','NH11','NH12','NH13','SH07','SH08','SH09','SH10','SH11','SH12','SH13']
shem=strmid(seasons,0,1)
nseasons=n_elements(seasons)

loadct,39
mcolor=255
!p.background=mcolor
device,decompose=0
nlvls=20L
col1=((1.+findgen(nlvls))/float(nlvls+1.))*mcolor

;******************** START LOOP OVER NODES ***********************
FOR INODE = 0,1 DO BEGIN
   IF INODE EQ 0 THEN SNODE = 'ASC' ELSE SNODE = 'DSC'

;******************** START LOOP OVER SEASONS *********************
FOR ISEA = 0,NSEASONS-1 DO BEGIN
   season=seasons(isea)

if setplot eq 'ps' then begin
SET_PLOT,'PS'
DEVICE,/LANDSCAPE
DEVICE,/COLOR,BITS_PER_PIXEL=8
DEVICE,/BOLD
DEVICE, FILENAME=ptho+'xt_cips_3c_'+season+'_'+snode+'_'+slat+'Lat_'+G+'G_albedo.ps'
endif

case season of
    'NH07': begin
    			sea='north_07'
    			iplot=0	;used to tell code to plot, not oplot
    			xi=0.85	;used for xyouts position
    		end
    'NH08': begin
    			sea='north_08'
     			iplot=2
    			xi=0.85
    		end
    'NH09': begin
    			sea='north_09'
    			iplot=2
    			xi=0.85
    		end
    'NH10': begin
    			sea='north_10'
    			iplot=2
    			xi=0.85
    		end
    'NH11': begin
    			sea='north_11'
    			iplot=2
    			xi=0.85
    		end
    'NH12': begin
                        sea='north_12'
                        iplot=2
                        xi=0.85
                end
    'NH13': begin
                        sea='north_13'
                        iplot=2
                        xi=0.85
                end
    'SH07': begin
    			sea='south_0708'
    			iplot=3
    			xi=0.85
    		end
    'SH08': begin
    			sea='south_0809'
    			iplot=3
    			xi=0.85
    		end
    'SH09': begin
    			sea='south_0910'
    			iplot=3
    			xi=0.85
    		end
    'SH10': begin
    			sea='south_1011'
    			iplot=3
    			xi=0.85
    		end
    'SH11': begin
                        sea='south_1112'
                        iplot=3
                        xi=0.85
                end
    'SH12': begin
                        sea='south_1213'
                        iplot=3
                        xi=0.85
                end
    'SH13': begin
                        sea='south_1314'
                        iplot=3
                        xi=0.85
                end
endcase

ver='04.20'

pre=['cips_3c_'+sea+'_v'+ver+'_r05_']
pre = replicate(pre,9)

case g of
   '1': fn=4
   '2': fn=5
   '5': fn=6
   '10': fn=7
endcase

SUBTI='v'+VER+', Rev05, '+G+'G'

suf = ['.sav']
suf = replicate(suf,9)

fnames=['01G_all','02G_all','05G_all','10G_all','01G_cld','02G_cld','05G_cld','10G_cld']
fnames=[fnames,'01G_nocld','02G_nocld','05G_nocld','10G_nocld']

fnames=pre+fnames+suf

FILL=-999

   restore,pth+fnames(fn)
;
; compute dfs
;
DFS=lonarr(nrev)
sdate=strcompress(date,/remove_all)
if strmid(season,0,2) eq 'SH' then jdaysol=JULDAY(12,21,long(strmid(sdate(0),0,4)))
if strmid(season,0,2) eq 'NH' then jdaysol=JULDAY(6,21,long(strmid(sdate(0),0,4)))
for i=0L,nrev-1L do begin
    iyr=long(strmid(sdate(i),0,4))
    imn=long(strmid(sdate(i),4,2))
    idy=long(strmid(sdate(i),6,2))
    dfs(i)=JULDAY(imn,idy,iyr)-jdaysol
endfor

   lat=1.0*lathi
   nlat=n_elements(lathi)
   for i=0,nlat-1 do lat(i)=(lathi(i)+latlo(i))/2.0
   ndays=max(dfs)-min(dfs)+1
   ddd = indgen(ndays)+min(dfs)
   array=fltarr(nlon,ndays)-99
   ilat=where(round(lat) eq slat)
   ilat=ilat(inode)     ;0 for ascending, 1 for descending

   for i=0,ndays-1 do begin
      x=where(dfs eq ddd(i),nx)
      if nx gt 0 then begin
         for j=0,nlon-1L do begin
            alb0=reform(alb(x,ilat))
            lon0=reform(lon(x,ilat))
            good=where(alb0 ne fill and lon0 ge lonbin(j)-dx/2. and lon0 lt lonbin(j)+dx/2.,ngood)
            if good(0) ne -1L then begin
               array(j,i)=alb0(good(0))
            if ngood gt 2 then begin
               array(j,i)=mean(alb0(good))
            endif
            endif
         endfor
      endif
   endfor

   bad=wherE(array eq -99.)
   if bad(0) ne -1L then array(bad)=0./0


;array(*,0:34) is ascending.
;array(*,35:69) is descending.
index=where(array gt 30.)
if index(0) ne -1L then array(index)=0./0.
erase
xmn=xorig(0)
xmx=xorig(0)+xlen
ymn=yorig(0)
ymx=yorig(0)+ylen
set_viewport,xmn,xmx,ymn,ymx
!type=2^2+2^3
level=findgen(nlvls)
contour,smooth(array,5,/NaN,/edge_truncate),lonbin,ddd,levels=level,c_color=col1,/cell_fill,xticks=6,$
        title=season+' '+SNODE+' '+slat+'!uo!n Lat',xtitle='Longitude',/noeras,color=0,xrange=[0,360],yrange=[10.,-50]
contour,smooth(array,5,/NaN,/edge_truncate),lonbin,ddd,levels=level,color=0,/overplot,/noeras,$
        c_charsize=2,c_labels=1+0*level

imin=min(level)
imax=max(level)
ymnb=yorig(0) -cbaryoff
ymxb=ymnb  +cbarydel
set_viewport,xmn,xmx,ymnb,ymxb
!type=2^2+2^3+2^6
plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],color=0,xtitle='CIPS Albedo (10!u-6!n sr!u-1!n) V4.20 Rev05'
ybox=[0,10,10,0,0]
x1=imin
ddx=(imax-imin)/float(nlvls)
for jj=0,nlvls-1 do begin
xbox=[x1,x1,x1+ddx,x1+ddx,x1]
polyfill,xbox,ybox,color=col1(jj)
x1=x1+ddx
endfor

if setplot ne 'ps' then stop
if setplot eq 'ps' then begin
DEVICE,/CLOSE
spawn,'convert -trim xt_cips_3c_'+season+'_'+snode+'_'+slat+'Lat_'+G+'G_albedo.ps -rotate -90 xt_cips_3c_'+season+'_'+snode+'_'+slat+'Lat_'+G+'G_albedo.png'
endif
;
; save data 
;
xtalbedo=array
save,file='xt_cips_3c_'+season+'_'+snode+'_'+slat+'Lat_'+G+'G_albedo.sav',xtalbedo,lonbin,ddd

ENDFOR
; ************ END LOOP OVER SEASONS ******************

ENDFOR
; ************ END LOOP OVER ASC/DSC ******************

end
