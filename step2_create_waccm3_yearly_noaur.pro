;
; create WACCM3 noaur yearly files
;
@stddat
@kgmt
@ckday
@kdate

dirh='/Volumes/Data/WACCM/WACCM3/noaurfco/noaurfco.cam2.h3.'
spawn,'ls '+dirh+'*0101.sav',ifiles
model_years=2025+3+findgen(n_elements(ifiles)-3)
model_years=string(FORMAT='(i4)',long(model_years))
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
; declare yearly average arrays
;
      if icount eq 0L then begin
         CLONO2NAFCO_avg=fltarr(nr,nl,kday)
         NOXNAFCO_avg=fltarr(nr,nl,kday)
         NOYNAFCO_avg=fltarr(nr,nl,kday)
         O3NAFCO_avg=fltarr(nr,nl,kday)
         OMEGANAFCO_avg=fltarr(nr,nl,kday)
         QSUMNAFCO_avg=fltarr(nr,nl,kday)
         TNAFCO_avg=fltarr(nr,nl,kday)
         UNAFCO_avg=fltarr(nr,nl,kday)
         VNAFCO_avg=fltarr(nr,nl,kday)
      endif
;
; loop over years and retain all data
;
      print,'restored '+dum(0)
      CLONO2NAFCO_avg(*,*,icount)=CLONO2NAFCO
      NOXNAFCO_avg(*,*,icount)=NOXNAFCO
      NOYNAFCO_avg(*,*,icount)=NOYNAFCO
      O3NAFCO_avg(*,*,icount)=O3NAFCO
      OMEGANAFCO_avg(*,*,icount)=OMEGANAFCO
      QSUMNAFCO_avg(*,*,icount)=QSUMNAFCO
      TNAFCO_avg(*,*,icount)=TNAFCO
      UNAFCO_avg(*,*,icount)=UNAFCO
      VNAFCO_avg(*,*,icount)=VNAFCO
;
jumpday:
      icount=icount+1L
goto,jump
;
; save yearly file
;
saveit:
ofile=dirh+'Year'+model_years(iyear)+'_VLH.sav'
save,file=ofile,sdate_all,lat,p,alt,CLONO2NAFCO_avg,NOXNAFCO_avg,NOYNAFCO_avg,O3NAFCO_avg,OMEGANAFCO_avg,$
     QSUMNAFCO_avg,TNAFCO_avg,UNAFCO_avg,VNAFCO_avg

endfor	; loop over years
end
