;
; create WACCM4 mee00 average year. Include averages and sigmas
;
@stddat
@kgmt
@ckday
@kdate

loadct,38
mcolor=byte(!p.color)
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
nxdim=700
nydim=700
xorig=[0.15]
yorig=[0.25]
xlen=0.7
ylen=0.5
dirh='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/mee00fpl_FW3.cam2.h3.'
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
icount=0L
;
; --- Loop here --------
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
;
; read WACCM data
;
      spawn,'ls '+dirh+'*_'+sdate+'_Qvars.sav',ifiles
      nyears=n_elements(ifiles)
      print,nyears,' years on '+sdate
      restore,ifiles(0)
      nr=n_elements(lat)
      nl=n_elements(lev)
;
; declare yearly average and sigma arrays
;
      if icount eq 0L then begin
         DTCORE00FPL_avg=fltarr(nr,nl,kday)
         QCP00FPL_avg=fltarr(nr,nl,kday)
         QRL00FPL_avg=fltarr(nr,nl,kday)
         QRLNLTE00FPL_avg=fltarr(nr,nl,kday)
         QRS00FPL_avg=fltarr(nr,nl,kday)
         QRS_AUR00FPL_avg=fltarr(nr,nl,kday)
         QRS_CO2NIR00FPL_avg=fltarr(nr,nl,kday)
         QRS_EUV00FPL_avg=fltarr(nr,nl,kday)
         QTHERMAL00FPL_avg=fltarr(nr,nl,kday)

         DTCORE00FPL_sig=fltarr(nr,nl,kday)
         QCP00FPL_sig=fltarr(nr,nl,kday)
         QRL00FPL_sig=fltarr(nr,nl,kday)
         QRLNLTE00FPL_sig=fltarr(nr,nl,kday)
         QRS00FPL_sig=fltarr(nr,nl,kday)
         QRS_AUR00FPL_sig=fltarr(nr,nl,kday)
         QRS_CO2NIR00FPL_sig=fltarr(nr,nl,kday)
         QRS_EUV00FPL_sig=fltarr(nr,nl,kday)
         QTHERMAL00FPL_sig=fltarr(nr,nl,kday)
      endif
;
; declare new "all" arrays every day
;
      DTCORE00FPL_all=fltarr(nr,nl,nyears)
      QCP00FPL_all=fltarr(nr,nl,nyears)
      QRL00FPL_all=fltarr(nr,nl,nyears)
      QRLNLTE00FPL_all=fltarr(nr,nl,nyears)
      QRS00FPL_all=fltarr(nr,nl,nyears)
      QRS_AUR00FPL_all=fltarr(nr,nl,nyears)
      QRS_CO2NIR00FPL_all=fltarr(nr,nl,nyears)
      QRS_EUV00FPL_all=fltarr(nr,nl,nyears)
      QTHERMAL00FPL_all=fltarr(nr,nl,nyears)
;
; loop over years and retain all data
;
      for iyear=0L,nyears-1L do begin
          restore,ifiles(iyear)
;         print,'restored '+ifiles(iyear)
          DTCORE00FPL_all(*,*,iyear)=DTCORE00FPL
          QCP00FPL_all(*,*,iyear)=QCP00FPL
          QRL00FPL_all(*,*,iyear)=QRL00FPL
          QRLNLTE00FPL_all(*,*,iyear)=QRLNLTE00FPL
          QRS00FPL_all(*,*,iyear)=QRS00FPL
          QRS_AUR00FPL_all(*,*,iyear)=QRS_AUR00FPL
          QRS_CO2NIR00FPL_all(*,*,iyear)=QRS_CO2NIR00FPL
          QRS_EUV00FPL_all(*,*,iyear)=QRS_EUV00FPL
          QTHERMAL00FPL_all(*,*,iyear)=QTHERMAL00FPL
      endfor
;
; fill yearly average and sigma arrays
;
      for j=0L,nr-1L do begin
          for k=0L,nl-1L do begin
              DTCORE00FPL_avg(j,k,icount)=mean(DTCORE00FPL_all(j,k,*))
              QCP00FPL_avg(j,k,icount)=mean(QCP00FPL_all(j,k,*))
              QRL00FPL_avg(j,k,icount)=mean(QRL00FPL_all(j,k,*))
              QRLNLTE00FPL_avg(j,k,icount)=mean(QRLNLTE00FPL_all(j,k,*))
              QRS00FPL_avg(j,k,icount)=mean(QRS00FPL_all(j,k,*))
              QRS_AUR00FPL_avg(j,k,icount)=mean(QRS_AUR00FPL_all(j,k,*))
              QRS_CO2NIR00FPL_avg(j,k,icount)=mean(QRS_CO2NIR00FPL_all(j,k,*))
              QRS_EUV00FPL_avg(j,k,icount)=mean(QRS_EUV00FPL_all(j,k,*))
              QTHERMAL00FPL_avg(j,k,icount)=mean(QTHERMAL00FPL_all(j,k,*))

              DTCORE00FPL_sig(j,k,icount)=stdev(DTCORE00FPL_all(j,k,*))
              QCP00FPL_sig(j,k,icount)=stdev(QCP00FPL_all(j,k,*))
              QRL00FPL_sig(j,k,icount)=stdev(QRL00FPL_all(j,k,*))
              QRLNLTE00FPL_sig(j,k,icount)=stdev(QRLNLTE00FPL_all(j,k,*))
              QRS00FPL_sig(j,k,icount)=stdev(QRS00FPL_all(j,k,*))
              QRS_AUR00FPL_sig(j,k,icount)=stdev(QRS_AUR00FPL_all(j,k,*))
              QRS_CO2NIR00FPL_sig(j,k,icount)=stdev(QRS_CO2NIR00FPL_all(j,k,*))
              QRS_EUV00FPL_sig(j,k,icount)=stdev(QRS_EUV00FPL_all(j,k,*))
              QTHERMAL00FPL_sig(j,k,icount)=stdev(QTHERMAL00FPL_all(j,k,*))

          endfor
      endfor
jump2count:
      icount=icount+1L
goto,jump
;
; save yearly average and sigma file
;
saveit:
ofile=dirh+'AverageYear_VLH_Qvars.sav'
print,ofile
save,file=ofile,lat,lev,DTCORE00FPL_avg,QCP00FPL_avg,QRL00FPL_avg,QRLNLTE00FPL_avg,QRS00FPL_avg,$
     QRS_AUR00FPL_avg,QRS_CO2NIR00FPL_avg,QRS_EUV00FPL_avg,QTHERMAL00FPL_avg,DTCORE00FPL_sig,QCP00FPL_sig,QRL00FPL_sig,$
     QRLNLTE00FPL_sig,QRS00FPL_sig,QRS_AUR00FPL_sig,QRS_CO2NIR00FPL_sig,QRS_EUV00FPL_sig,QTHERMAL00FPL_sig
end
