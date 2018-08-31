;
; create WACCM4 noaur average year. Include averages and sigmas
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
dirh='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/noaurfpl_FW3.cam2.h3.'
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
         DTCORENAFPL_avg=fltarr(nr,nl,kday)
         QCPNAFPL_avg=fltarr(nr,nl,kday)
         QRLNAFPL_avg=fltarr(nr,nl,kday)
         QRLNLTENAFPL_avg=fltarr(nr,nl,kday)
         QRSNAFPL_avg=fltarr(nr,nl,kday)
         QRS_AURNAFPL_avg=fltarr(nr,nl,kday)
         QRS_CO2NIRNAFPL_avg=fltarr(nr,nl,kday)
         QRS_EUVNAFPL_avg=fltarr(nr,nl,kday)
         QTHERMALNAFPL_avg=fltarr(nr,nl,kday)

         DTCORENAFPL_sig=fltarr(nr,nl,kday)
         QCPNAFPL_sig=fltarr(nr,nl,kday)
         QRLNAFPL_sig=fltarr(nr,nl,kday)
         QRLNLTENAFPL_sig=fltarr(nr,nl,kday)
         QRSNAFPL_sig=fltarr(nr,nl,kday)
         QRS_AURNAFPL_sig=fltarr(nr,nl,kday)
         QRS_CO2NIRNAFPL_sig=fltarr(nr,nl,kday)
         QRS_EUVNAFPL_sig=fltarr(nr,nl,kday)
         QTHERMALNAFPL_sig=fltarr(nr,nl,kday)
      endif
;
; declare new "all" arrays every day
;
      DTCORENAFPL_all=fltarr(nr,nl,nyears)
      QCPNAFPL_all=fltarr(nr,nl,nyears)
      QRLNAFPL_all=fltarr(nr,nl,nyears)
      QRLNLTENAFPL_all=fltarr(nr,nl,nyears)
      QRSNAFPL_all=fltarr(nr,nl,nyears)
      QRS_AURNAFPL_all=fltarr(nr,nl,nyears)
      QRS_CO2NIRNAFPL_all=fltarr(nr,nl,nyears)
      QRS_EUVNAFPL_all=fltarr(nr,nl,nyears)
      QTHERMALNAFPL_all=fltarr(nr,nl,nyears)
;
; loop over years and retain all data
;
      for iyear=0L,nyears-1L do begin
          restore,ifiles(iyear)
;         print,'restored '+ifiles(iyear)
          DTCORENAFPL_all(*,*,iyear)=DTCORENAFPL
          QCPNAFPL_all(*,*,iyear)=QCPNAFPL
          QRLNAFPL_all(*,*,iyear)=QRLNAFPL
          QRLNLTENAFPL_all(*,*,iyear)=QRLNLTENAFPL
          QRSNAFPL_all(*,*,iyear)=QRSNAFPL
          QRS_AURNAFPL_all(*,*,iyear)=QRS_AURNAFPL
          QRS_CO2NIRNAFPL_all(*,*,iyear)=QRS_CO2NIRNAFPL
          QRS_EUVNAFPL_all(*,*,iyear)=QRS_EUVNAFPL
          QTHERMALNAFPL_all(*,*,iyear)=QTHERMALNAFPL
      endfor
;
; fill yearly average and sigma arrays
;
      for j=0L,nr-1L do begin
          for k=0L,nl-1L do begin
              DTCORENAFPL_avg(j,k,icount)=mean(DTCORENAFPL_all(j,k,*))
              QCPNAFPL_avg(j,k,icount)=mean(QCPNAFPL_all(j,k,*))
              QRLNAFPL_avg(j,k,icount)=mean(QRLNAFPL_all(j,k,*))
              QRLNLTENAFPL_avg(j,k,icount)=mean(QRLNLTENAFPL_all(j,k,*))
              QRSNAFPL_avg(j,k,icount)=mean(QRSNAFPL_all(j,k,*))
              QRS_AURNAFPL_avg(j,k,icount)=mean(QRS_AURNAFPL_all(j,k,*))
              QRS_CO2NIRNAFPL_avg(j,k,icount)=mean(QRS_CO2NIRNAFPL_all(j,k,*))
              QRS_EUVNAFPL_avg(j,k,icount)=mean(QRS_EUVNAFPL_all(j,k,*))
              QTHERMALNAFPL_avg(j,k,icount)=mean(QTHERMALNAFPL_all(j,k,*))

              DTCORENAFPL_sig(j,k,icount)=stdev(DTCORENAFPL_all(j,k,*))
              QCPNAFPL_sig(j,k,icount)=stdev(QCPNAFPL_all(j,k,*))
              QRLNAFPL_sig(j,k,icount)=stdev(QRLNAFPL_all(j,k,*))
              QRLNLTENAFPL_sig(j,k,icount)=stdev(QRLNLTENAFPL_all(j,k,*))
              QRSNAFPL_sig(j,k,icount)=stdev(QRSNAFPL_all(j,k,*))
              QRS_AURNAFPL_sig(j,k,icount)=stdev(QRS_AURNAFPL_all(j,k,*))
              QRS_CO2NIRNAFPL_sig(j,k,icount)=stdev(QRS_CO2NIRNAFPL_all(j,k,*))
              QRS_EUVNAFPL_sig(j,k,icount)=stdev(QRS_EUVNAFPL_all(j,k,*))
              QTHERMALNAFPL_sig(j,k,icount)=stdev(QTHERMALNAFPL_all(j,k,*))

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
save,file=ofile,lat,lev,DTCORENAFPL_avg,QCPNAFPL_avg,QRLNAFPL_avg,QRLNLTENAFPL_avg,QRSNAFPL_avg,$
     QRS_AURNAFPL_avg,QRS_CO2NIRNAFPL_avg,QRS_EUVNAFPL_avg,QTHERMALNAFPL_avg,DTCORENAFPL_sig,QCPNAFPL_sig,QRLNAFPL_sig,$
     QRLNLTENAFPL_sig,QRSNAFPL_sig,QRS_AURNAFPL_sig,QRS_CO2NIRNAFPL_sig,QRS_EUVNAFPL_sig,QTHERMALNAFPL_sig
end
