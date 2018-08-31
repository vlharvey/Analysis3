;plt_l4_summary.pro -- Make example plots to check out the level 4 summary files.

print,'MUST DEFINE DFS FOR NH11 SEASON AND BEYOND!!!'
PRINT,' '
stop

;FOR FREQUENCY, WE ONLY NEED THE "ALL" FILES.

;THIS CODE WORKS WITH THE 1-DEGREE LATITUDE BIN FILES.
;LATITUDES GO FROM 50-51 TO 84-85 AND 85-84 TO 51-50, IN 1-DEGREE INCREMENTS.

pth = 'C:\CIPS\data\v4.20\level3c\'
ptho = 'C:\CIPS\analysis\v4.20\line_plots\'

season='north_07'
read,'Input the season (nh07, nh08, nh09, nh10, sh07, sh08, sh09, sh10) >> ',season
season=strupcase(season)
case season of
    'NH07': sea='north_07'
    'NH08': sea='north_08'
    'NH09': sea='north_09'
    'NH10': sea='north_10'
    'SH07': sea='south_0708'
    'SH08': sea='south_0809'
    'SH09': sea='south_0910'
    'SH10': sea='south_1011'
endcase

ver='04.20'

pre=['cips_3c_'+sea+'_v'+ver+'_r04_']
pre = replicate(pre,9)


Gs = ['1','2','5']
ng=n_elements(gs)

;*************** BEGIN LOOP OVER DIFFERENT THRESHOLDS *******************
FOR THRESH = 0,ng-1 DO BEGIN

g = Gs(thresh)

case g of
   '1': fn=4
   '2': fn=5
   '5': fn=6
   '10': fn=7
endcase

SUBTI=season+', v'+VER+', Rev04, '+G+'G'

suf = ['.sav']
suf = replicate(suf,9)

fnames=['01G_all','02G_all','05G_all','10G_all','01G_cld','02G_cld','05G_cld','10G_cld']
fnames=[fnames,'01G_nocld','02G_nocld','05G_nocld','10G_nocld']

fnames=pre+fnames+suf

FILL=-999

if sea eq 'north_07' then hem='NH'
if sea eq 'north_08' then hem='NH'
if sea eq 'north_09' then hem='NH'
if sea eq 'north_10' then hem='NH'
if sea eq 'south_0708' then hem='SH'
if sea eq 'south_0809' then hem='SH'
if sea eq 'south_0910' then hem='SH'
if sea eq 'south_1011' then hem='SH'

if sea eq 'north_07' then shem='N'
if sea eq 'north_08' then shem='N'
if sea eq 'north_09' then shem='N'
if sea eq 'north_10' then shem='N'
if sea eq 'south_0708' then shem='S'
if sea eq 'south_0809' then shem='S'
if sea eq 'south_0910' then shem='S'
if sea eq 'south_1011' then shem='S'

   restore,pth+fnames(fn)
   lat=1.0*lathi
   nlat=n_elements(lathi)
   for i=0,nlat-1 do lat(i)=(lathi(i)+latlo(i))/2.0
   ndays=max(dfs)-min(dfs)+1
   ddd = indgen(ndays)+min(dfs)
   array=fltarr(ndays,nlat)-99
   for i=0,ndays-1 do begin
      x=where(dfs eq ddd(i),nx)
      if nx gt 0 then begin
         for j=0,nlat-1 do begin
            good=where(iwc(x,j) ne fill,ngood)
            if ngood gt 2 then begin
               array(i,j)=mean(iwc(x(good),j))
            endif
         endfor
      endif
   endfor

   bad=wherE(array eq -99)
   array(bad)=0./0

SET_PLOT,'PS'
DEVICE,/LANDSCAPE
DEVICE,/COLOR,BITS_PER_PIXEL=8
DEVICE,/BOLD
DEVICE, FILENAME=ptho+'I_V4.20_'+season+'_'+G+'G.ps'

restore,'c:\idl_cora\c11.tbl'
tvlct,c1,c2,c3

!p.charsize=1.2
!P.FONT=0
!P.THICK=3
!p.multi=[0,2,2]

!Y.THICK=3
!y.style=1
!y.range=[0,180]
!y.title='Ice Water Content (!mm!xg m!u-2!n)'
!Y.MARGIN=3

!X.THICK=3
!x.style=1
!x.range=[-40,80]
;!x.range=[20,33]
!x.title='Days From Solstice'
!X.MARGIN=4

;array(*,0:34) is ascending.
;array(*,35:69) is descending.

   ;if hem eq 'SH' then lat = (-1.0)*lat
   ;Don't need to do this since latitudes in the summary files are all +

   lats=['80','75','70','65']
   cols=[0,11,3,4]
   nlats=n_elements(lats)

   plot,[0,0],[0,0],/nodata,title='Ascending IWC'
   for ll=0,nlats-1 do begin
      li=where(round(lat) eq lats(ll))
      li=li(0)	;for ascending
      oplot,ddd,array(*,li),thick=5,color=cols(ll)
      xyouts,0.4,0.88-0.03*ll,lats(ll)+'!uo!n'+shem,color=cols(ll),/normal,charsize=1.2
   endfor

   plot,[0,0],[0,0],/nodata,title='Descending IWC'
   for ll=0,nlats-1 do begin
      li=where(round(lat) eq lats(ll))
      li=li(1)	;for descending
      oplot,ddd,array(*,li),thick=5,color=cols(ll)
      xyouts,0.9,0.88-0.03*ll,lats(ll)+'!uo!n'+shem,color=cols(ll),/normal,charsize=1.2
   endfor

   plot,[0,0],[0,0],/nodata,title='IWC, 85!uo!n'+shem
   li=where(round(lat) eq 85)
   oplot,ddd,array(*,li(0)),thick=5,color=11
   oplot,ddd,array(*,li(1)),thick=5,color=3
   xyouts,0.4,0.38,'Asc',color=11,/normal,charsize=1.2
   xyouts,0.4,0.35,'Dsc',color=3,/normal,charsize=1.2

   plot,[0,0],[0,0],/nodata,title='IWC, 70!uo!n'+shem
   li=where(round(lat) eq 70)
   oplot,ddd,array(*,li(0)),thick=5,color=11
   oplot,ddd,array(*,li(1)),thick=5,color=3
   xyouts,0.9,0.38,'Asc',color=11,/normal,charsize=1.2
   xyouts,0.9,0.35,'Dsc',color=3,/normal,charsize=1.2

   xyouts,0.5,1.0,subti,alignment=0.5,charsize=1.5,/normal


DEVICE,/CLOSE
SET_PLOT,'WIN'

ENDFOR
; ********************* END LOOP OVER THRESHOLDS ********************

end
