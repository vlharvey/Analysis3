;
; create WACCM4 noaur yearly files
;
@stddat
@kgmt
@ckday
@kdate

dirh='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW/noaurfpl_FW.cam2.h3.'
model_years=3+findgen(20)
model_years=string(FORMAT='(i2.2)',long(model_years))
nyears=n_elements(model_years)
;
; loop over model years
;
for iyear=0L,nyears-1L do begin

lstmn=1 & lstdy=1 & lstyr=1995 & lstday=0
ledmn=12 & leddy=31 & ledyr=1995 & ledday=0	; choose any non leap year
z = stddat(lstmn,lstdy,lstyr,lstday)
z = stddat(ledmn,leddy,ledyr,ledday)
if ledday lt lstday then stop,' Wrong dates! '
iyr = lstyr
idy = lstdy
imn = lstmn
z = kgmt(imn,idy,iyr,iday)
iday = iday - 1
kday=ledday-lstday+1L
if kday ne 365L then stop,'check kday'
sdate_all=strarr(kday)
icount=0
;
; loop over days
;
jump: iday = iday + 1
      kdate,float(iday),iyr,imn,idy
      ckday,iday,iyr
;
; test for end condition and close windows.
;
      z = stddat(imn,idy,iyr,ndays)
      if ndays lt lstday then stop,' starting day outside range '
      if icount eq kday then goto,saveit
      sdy=string(FORMAT='(i2.2)',idy)
      smn=string(FORMAT='(i2.2)',imn)
      sdate=smn+sdy
      sdate_all(icount)=sdate
      if sdate eq '0229' then stop,'check leap year?'
;
; read WACCM data
;
      dum=findfile(dirh+'Year'+model_years(iyear)+'_'+sdate+'.sav')
      if dum(0) eq '' then goto,jumpday
      restore,dum(0)
      nr=n_elements(lat)
      nl=n_elements(p)
;
; declare yearly average and sigma arrays
;
      if icount eq 0L then begin
         CLONO2NAFPL_avg=fltarr(nr,nl,kday)
         NOXNAFPL_avg=fltarr(nr,nl,kday)
         NOYNAFPL_avg=fltarr(nr,nl,kday)
         O3NAFPL_avg=fltarr(nr,nl,kday)
         OMEGANAFPL_avg=fltarr(nr,nl,kday)
         QSUMNAFPL_avg=fltarr(nr,nl,kday)
         TNAFPL_avg=fltarr(nr,nl,kday)
         UNAFPL_avg=fltarr(nr,nl,kday)
         VNAFPL_avg=fltarr(nr,nl,kday)
      endif
;
; loop over years and retain all data
;
      print,'restored '+dum(0)
      CLONO2NAFPL_avg(*,*,icount)=CLONO2NAFPL
      NOXNAFPL_avg(*,*,icount)=NOXNAFPL
      NOYNAFPL_avg(*,*,icount)=NOYNAFPL
      O3NAFPL_avg(*,*,icount)=O3NAFPL
      OMEGANAFPL_avg(*,*,icount)=OMEGANAFPL
      QSUMNAFPL_avg(*,*,icount)=QSUMNAFPL
      TNAFPL_avg(*,*,icount)=TNAFPL
      UNAFPL_avg(*,*,icount)=UNAFPL
      VNAFPL_avg(*,*,icount)=VNAFPL
;
jumpday:
      icount=icount+1L
goto,jump
;
; save yearly file
;
saveit:
ofile=dirh+'Year'+model_years(iyear)+'_VLH.sav'
save,file=ofile,sdate_all,lat,p,alt,CLONO2NAFPL_avg,NOXNAFPL_avg,NOYNAFPL_avg,O3NAFPL_avg,OMEGANAFPL_avg,$
     QSUMNAFPL_avg,TNAFPL_avg,UNAFPL_avg,VNAFPL_avg

endfor	; loop over years
end
