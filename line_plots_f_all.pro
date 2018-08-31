;plt_l4_summary.pro -- Make example plots to check out the level 4 summary files.

;print,' '
;print,'MUST DEFINE DFS FOR NH11 SEASON AND BEYOND!!!'
;PRINT,' '

;stop

;FOR FREQUENCY, WE ONLY NEED THE "ALL" FILES.

;THIS CODE WORKS WITH THE 1-DEGREE LATITUDE BIN FILES.
;LATITUDES GO FROM 50-51 TO 84-85 AND 85-84 TO 51-50, IN 1-DEGREE INCREMENTS.

pth = 'C:\CIPS\data\v4.20\level3c\'
ptho = 'C:\CIPS\analysis\v4.20\line_plots\'

G = '1'
read,'Input the threshold (1,2,5) >> ',G

slat='85'
;read,'Input the latitude (positive) >> ',slat

SET_PLOT,'PS'
DEVICE,/LANDSCAPE
DEVICE,/COLOR,BITS_PER_PIXEL=8
DEVICE,/BOLD
DEVICE, FILENAME=ptho+'F_V4.20_'+slat+'Lat_'+G+'G_all.ps'

restore,'c:\idl_cora\c11.tbl'
tvlct,c1,c2,c3

!p.charsize=1.8
!P.FONT=0
!P.THICK=3
!p.multi=0

!Y.THICK=3
!y.style=1
!y.range=[0,100]
!y.title='Frequency (%)'
;!Y.MARGIN=3

!X.THICK=3
!x.style=1
!x.range=[-40,80]
;!x.range=[20,33]
!x.title='Days From Solstice'
;!X.MARGIN=4

seasons=['NH07','NH08','NH09','NH10','NH11','SH07','SH08','SH09','SH10']
shem=strmid(seasons,0,1)
nseasons=n_elements(seasons)

cols=[0,1,2,3,4,11,10,9,8]

;******************** START LOOP OVER NODES ***********************
FOR INODE = 0,1 DO BEGIN
   IF INODE EQ 0 THEN SNODE = 'ASC' ELSE SNODE = 'DSC'

;******************** START LOOP OVER SEASONS *********************
FOR ISEA = 0,NSEASONS-1 DO BEGIN
   season=seasons(isea)

case season of
    'NH07': begin
    			sea='north_07'
    			iplot=0	;used to tell code to plot, not oplot
    			xi=0.85	;used for xyouts position
    			sol=172
    			test=0	;For doy: if NH11 or later, must calculate doy
    		end
    'NH08': begin
    			sea='north_08'
     			iplot=2
    			xi=0.85
    			sol=173
    			test=0
    		end
    'NH09': begin
    			sea='north_09'
    			iplot=2
    			xi=0.85
    			sol=172
    			test=0
    		end
    'NH10': begin
    			sea='north_10'
    			iplot=2
    			xi=0.85
    			sol=172
    			test=0
    		end
    'NH11': begin
    			sea='north_11'
    			iplot=2
    			xi=0.85
    			sol=172
    			test=1
    		end
    'SH07': begin
    			sea='south_0708'
    			iplot=3
    			xi=0.85
    			sol=355
    			dadd=365
    			test=0
    		end
    'SH08': begin
    			sea='south_0809'
    			iplot=3
    			xi=0.85
    			sol=356
    			dadd=366
    			test=0
    		end
    'SH09': begin
    			sea='south_0910'
    			iplot=3
    			xi=0.85
    			sol=355
    			dadd=365
    			test=0
    		end
    'SH10': begin
    			sea='south_1011'
    			iplot=3
    			xi=0.85
    			sol=355
    			dadd=365
    			test=0
    		end
endcase

ver='04.20'

pre=['cips_3c_'+sea+'_v'+ver+'_r04_']
pre = replicate(pre,9)

case g of
   '1': fn=0
   '2': fn=1
   '5': fn=2
   '10': fn=3
endcase

SUBTI='v'+VER+', Rev04, '+G+'G'

suf = ['.sav']
suf = replicate(suf,9)

fnames=['01G_all','02G_all','05G_all','10G_all','01G_cld','02G_cld','05G_cld','10G_cld']
fnames=[fnames,'01G_nocld','02G_nocld','05G_nocld','10G_nocld']

fnames=pre+fnames+suf

FILL=-999

   restore,pth+fnames(fn)
   if test eq 1 then begin
      jday=yyyymmdd_to_jday(date)
      yyyy=strcompress(date(0),/remove_all)
      yyyy=strmid(yyyy,0,4)
      doy=jday-julday(12,31,yyyy-1)
   endif
   dfs=doy-sol
   bad=wherE(doy ge 0 and doy lt 100,nbad)	;Jan-Mar
   if nbad gt 0 then dfs(bad)=dfs(bad)+dadd
   lat=1.0*lathi
   nlat=n_elements(lathi)
   for i=0,nlat-1 do lat(i)=(lathi(i)+latlo(i))/2.0
   ndays=max(dfs)-min(dfs)+1
   ddd = indgen(ndays)+min(dfs)
   array=fltarr(ndays,nlat)-99
   totclouds=array
   totobs=array
   for i=0,ndays-1 do begin
      x=where(dfs eq ddd(i),nx)
      if nx gt 0 then begin
         cld=num_cld(x,*) & obs=num_obs(x,*)
         for j=0,nlat-1 do begin
            good=where(cld(*,j) ne fill and obs(*,j) gt 0,ngood)
            if ngood gt 0 then begin
               array(i,j)=total(cld(good,j))/total(obs(good,j))
               totclouds(i,j)=total(cld(good,j))
               totobs(i,j)=total(obs(good,j))
            endif
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
      title=SNODE+' Frequency, '+slat+'!uo!n Lat',subtitle=subti
   ilat=where(round(lat) eq slat)
   ilat=ilat(inode)	;0 for ascending, 1 for descending

   oplot,ddd,smooth(100.*array(*,ilat),5,/nan),thick=8,color=cols(isea)
   xyouts,xi,0.88-0.04*isea,seasons(isea),color=cols(isea),/normal,charsize=1.8

   ;xyouts,0.5,1.0,subti,alignment=0.5,charsize=1.5,/normal

ENDFOR
; ************ END LOOP OVER SEASONS ******************

ENDFOR
; ************ END LOOP OVER ASC/DSC ******************

DEVICE,/CLOSE
SET_PLOT,'WIN'


end
