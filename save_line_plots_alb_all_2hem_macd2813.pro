;
; save Line_Plots/ALB_V4.20_r05_80Lat_2G_all_sh_plot_data.sav
; save Line_plots/ALB_V4.20_r05_80Lat_2G_all_nh_plot_data.sav
;
; that contain:
;
; PLOT_DAYS_ASC   INT       = Array[120, 7]
; PLOT_DAYS_DSC   INT       = Array[120, 7]
; PLOT_ALB_ASC      FLOAT     = Array[120, 7]
; PLOT_ALB_DSC      FLOAT     = Array[120, 7]
; SEASONS         STRING    = Array[7]
; SLAT            STRING    = '80'
;
;plt_l4_summary.pro -- Make example plots to check out the level 4 summary files.

@stddat
@kgmt
@ckday
@kdate

;print,'MUST DEFINE DFS FOR NH11 SEASON AND BEYOND!!!'
;PRINT,' '
;stop

;FOR FREQUENCY, WE ONLY NEED THE "ALL" FILES.
;FOR ALBEDO WE NEED CLD FILES

;THIS CODE WORKS WITH THE 1-DEGREE LATITUDE BIN FILES.
;LATITUDES GO FROM 50-51 TO 84-85 AND 85-84 TO 51-50, IN 1-DEGREE INCREMENTS.

pth = '/Volumes/Data/CIPS_data/Datfiles/'
ptho = '/Volumes/Data/CIPS_data/Pre_process/Line_plots/'

G = '2'
;read,'Input the threshold (1,2,5) >> ',G

slat='85'
;read,'Input the latitude (positive) >> ',slat


HEMS=['NH','SH']

setplot='ps'
read,'setplot=',setplot

if setplot eq 'ps' then begin
SET_PLOT,'PS'
DEVICE,/LANDSCAPE
DEVICE,/COLOR,BITS_PER_PIXEL=8
DEVICE,/BOLD
DEVICE, FILENAME=ptho+'ALB_V4.20_'+slat+'Lat_'+G+'G_2hem.ps'
endif
if setplot ne 'ps' then begin
   window,4,xsize=750,ysize=750,retain=2,colors=162
   !p.background=255
endif

restore,'c11.tbl'
tvlct,c1,c2,c3
device,decompose=0

!p.charsize=1.2
!P.FONT=0
!P.THICK=3
!p.multi=[0,2,2]

!Y.THICK=3
!y.style=1
!y.range=[0,30]
!y.title='Albedo (G)'
!Y.MARGIN=3

!X.THICK=3
!x.style=1
!x.range=[-40,80]
;!x.range=[20,33]
!x.title='Days From Solstice'
!X.MARGIN=4

;************** START LOOP OVER HEMISPHERES **********************
FOR IHEM=0,1 DO BEGIN
   HEM=HEMS(iHEM)

seasons_nh=['NH07','NH08','NH09','NH10','NH11','NH12','NH13','NH14']
seasons_sh=['SH07','SH08','SH09','SH10','SH11','SH12','SH13','SH14']
if hem eq 'NH' then begin
   seasons=seasons_nh
   shem='N'
   jdaysol=172.
   col1=[0,150,1,2,3,4,5,11]
   outfile=ptho+'ALB_V4.20_'+slat+'Lat_'+G+'G_all_nh_plot_data.sav'
endif else begin
   seasons=seasons_sh
   shem='S'
   jdaysol=355.
   col1=[0,150,1,3,5,6,9,11]
   outfile=ptho+'ALB_V4.20_'+slat+'Lat_'+G+'G_all_sh_plot_data.sav'
endelse
nseasons=n_elements(seasons)

;******************** START LOOP OVER SEASONS *********************
FOR ISEA = 0,NSEASONS-1 DO BEGIN
   season=seasons(isea)
   print,season

case season of
    'NH07': begin
    			sea='north_07'
    			iplot=0	;used to tell code to plot, not oplot
    			xi=0.4	;used for xyouts position
    		end
    'NH08': begin
    			sea='north_08'
     			iplot=2
    			xi=0.4
    		end
    'NH09': begin
    			sea='north_09'
    			iplot=2
    			xi=0.4
    		end
    'NH10': begin
    			sea='north_10'
    			iplot=2
    			xi=0.4
    		end
    'NH11': begin
                        sea='north_11'
                        iplot=2
                        xi=0.4
                end
    'NH12': begin
                        sea='north_12'
                        iplot=2
                        xi=0.4
                end
    'NH13': begin
                        sea='north_13'
                        iplot=2
                        xi=0.4
                end
    'NH14': begin
                        sea='north_14'
                        iplot=2
                        xi=0.4
                end
    'SH07': begin
    			sea='south_0708'
    			iplot=0
    			xi=0.9
    		end
    'SH08': begin
    			sea='south_0809'
    			iplot=2
    			xi=0.9
    		end
    'SH09': begin
    			sea='south_0910'
    			iplot=2
    			xi=0.9
    		end
    'SH10': begin
    			sea='south_1011'
    			iplot=2
    			xi=0.9
    		end
    'SH11': begin
                        sea='south_1112'
                        iplot=2
                        xi=0.9
                end
    'SH12': begin
                        sea='south_1213'
                        iplot=2
                        xi=0.9
                end
    'SH13': begin
                        sea='south_1314'
                        iplot=2
                        xi=0.9
                end
    'SH14': begin
                        sea='south_1415'
                        iplot=2
                        xi=0.9
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
; convert DATE to DFS
;
nn=n_elements(date)
fdoy=fltarr(nn)
dfs=fltarr(nn)
for i=0L,nn-1L do begin
    iyr=long(strmid(strcompress(date(i),/remove_all),0,4))
    imn=long(strmid(strcompress(date(i),/remove_all),4,2))
    idy=long(strmid(strcompress(date(i),/remove_all),6,2))
    z = kgmt(imn,idy,iyr,iday)
    fdoy(i)=1.0*iday
    dfs(i)=fdoy(i)-jdaysol
endfor

if shem eq 'S' then begin
dum=dfs
maxday=max(dum)
index=where(dum lt dum(0))
dum(index)=maxday+1+dum(index)+abs(min(dum(index)))
dfs=dum
endif

   lat=1.0*lathi
   nlat=n_elements(lathi)
   for i=0,nlat-1 do lat(i)=(lathi(i)+latlo(i))/2.0
   ndays=max(dfs)-min(dfs)+1
   ddd = indgen(ndays)+min(dfs)
   array=fltarr(ndays,nlat)-99
if isea eq 0L then begin
   PLOT_ALB_ASC=fltarr(130,nseasons)
   PLOT_ALB_DSC=fltarr(130,nseasons)
   PLOT_DAYS_ASC=fltarr(130,nseasons)
   PLOT_DAYS_DSC=fltarr(130,nseasons)
endif
   totclouds=array
   totobs=array
   albedo=array
   for i=0,ndays-1 do begin
      x=where(dfs eq ddd(i),nx)
if nx eq 0 then print,'missing '+season,ddd(i)
      if nx gt 0 then begin
         cld=num_cld(x,*) & obs=num_obs(x,*)
         for j=0,nlat-1 do begin
            good=where(cld(*,j) ne fill and obs(*,j) gt 0,ngood)
            if ngood gt 0 then begin
               array(i,j)=total(cld(good,j))/total(obs(good,j))
               totclouds(i,j)=total(cld(good,j))
               totobs(i,j)=total(obs(good,j))
            endif

            alb0=reform(alb(x,j))	; today's albedos at latitude j
            good=where(alb0 ne fill and alb0 ne 0,ngood)
            if good(0) ne -1L then begin
               albedo(i,j)=alb0(good(0))
            endif
            if ngood gt 2 then begin
               albedo(i,j)=mean(alb0(good))
            endif

;if lathi(j) eq long(slat) then print,ddd(i),lathi(j),albedo(i,j)
         endfor
      endif
   endfor
   bad=wherE(array eq -99)
   array(bad)=0./0

;array(*,0:34) is ascending.
;array(*,35:69) is descending.

   ;if hem eq 'SH' then lat = (-1.0)*lat
   ;Don't need to do this since latitudes in the summary files are all +

   if iplot eq 0 then plot,[0,0],[0,0],/nodata, $
      title='Asc Albedo, '+slat+'!uo!n'+shem,color=0
   ilat=where(round(lat) eq slat)
bad=where(albedo eq -99.)
albedo(bad)=0./0.
PLOT_ALB_ASC(0:ndays-1,isea)=smooth(albedo(*,ilat(0)),3,/Nan)
PLOT_ALB_DSC(0:ndays-1,isea)=smooth(albedo(*,ilat(1)),3,/Nan)
PLOT_DAYS_ASC(0:ndays-1,isea)=ddd
PLOT_DAYS_DSC(0:ndays-1,isea)=ddd

   ilat=ilat(0)	;for ascending

;  cols=[0,11,3,1]

   oplot,ddd,smooth(albedo(*,ilat),3,/Nan),thick=5,color=col1(isea)
   xyouts,xi,0.88-0.03*isea,seasons(isea),color=col1(isea),/normal,charsize=1.2

   xyouts,0.5,.5,subti,alignment=0.5,charsize=1.5,/normal,color=0
print,season,ndays

save,file=ptho+'ALB_V4.20_'+slat+'Lat_'+G+'G_'+season+'.sav',ddd,albedo
ENDFOR
; ************ END LOOP OVER SEASONS ******************
;
; save all seasons in one file
;
save,file=outfile,PLOT_DAYS_ASC,PLOT_DAYS_DSC,PLOT_ALB_ASC,PLOT_ALB_DSC,SEASONS,SLAT


ENDFOR
;************* END LOOP OVER HEMISPHERES ***************

if setplot eq 'ps' then device, /close

end
