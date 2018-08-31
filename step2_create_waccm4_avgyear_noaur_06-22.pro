;
; create WACCM4 noaur average year. Include averages and sigmas
; eliminate first 5 years
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
dirh='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW/noaurfpl_FW.cam2.h3.'
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
      if ndays gt ledday then goto,saveit
      sdy=string(FORMAT='(i2.2)',idy)
      smn=string(FORMAT='(i2.2)',imn)
      sdate=smn+sdy
      print,sdate
;
; read WACCM data
;
      spawn,'ls '+dirh+'*_'+sdate+'.sav',ifiles
      nyears=n_elements(ifiles)
;     if nyears ne 20L then stop,'check number of years on '+sdate
      restore,ifiles(0)
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
         CLONO2NAFPL_sig=fltarr(nr,nl,kday)
         NOXNAFPL_sig=fltarr(nr,nl,kday)
         NOYNAFPL_sig=fltarr(nr,nl,kday)
         O3NAFPL_sig=fltarr(nr,nl,kday)
         OMEGANAFPL_sig=fltarr(nr,nl,kday)
         QSUMNAFPL_sig=fltarr(nr,nl,kday)
         TNAFPL_sig=fltarr(nr,nl,kday)
         UNAFPL_sig=fltarr(nr,nl,kday)
         VNAFPL_sig=fltarr(nr,nl,kday)
      endif
;
; declare new "all" arrays every day
;
      CLONO2NAFPL_all=fltarr(nr,nl,nyears)
      NOXNAFPL_all=fltarr(nr,nl,nyears)
      NOYNAFPL_all=fltarr(nr,nl,nyears)
      O3NAFPL_all=fltarr(nr,nl,nyears)
      OMEGANAFPL_all=fltarr(nr,nl,nyears)
      QSUMNAFPL_all=fltarr(nr,nl,nyears)
      TNAFPL_all=fltarr(nr,nl,nyears)
      UNAFPL_all=fltarr(nr,nl,nyears)
      VNAFPL_all=fltarr(nr,nl,nyears)
;
; loop over years and retain all data
;
      for iyear=0L,nyears-1L do begin
          restore,ifiles(iyear)
          print,'restored '+ifiles(iyear)
          CLONO2NAFPL_all(*,*,iyear)=CLONO2NAFPL
          NOXNAFPL_all(*,*,iyear)=NOXNAFPL
          NOYNAFPL_all(*,*,iyear)=NOYNAFPL
          O3NAFPL_all(*,*,iyear)=O3NAFPL
          OMEGANAFPL_all(*,*,iyear)=OMEGANAFPL
          QSUMNAFPL_all(*,*,iyear)=QSUMNAFPL
          TNAFPL_all(*,*,iyear)=TNAFPL
          UNAFPL_all(*,*,iyear)=UNAFPL
          VNAFPL_all(*,*,iyear)=VNAFPL
      endfor
;
; fill yearly average and sigma arrays
;
      for j=0L,nr-1L do begin
          for k=0L,nl-1L do begin
              CLONO2NAFPL_avg(j,k,icount)=mean(CLONO2NAFPL_all(j,k,*))
              NOXNAFPL_avg(j,k,icount)=mean(NOXNAFPL_all(j,k,*))
              NOYNAFPL_avg(j,k,icount)=mean(NOYNAFPL_all(j,k,*))
              O3NAFPL_avg(j,k,icount)=mean(O3NAFPL_all(j,k,*))
              OMEGANAFPL_avg(j,k,icount)=mean(OMEGANAFPL_all(j,k,*))
              QSUMNAFPL_avg(j,k,icount)=mean(QSUMNAFPL_all(j,k,*))
              TNAFPL_avg(j,k,icount)=mean(TNAFPL_all(j,k,*))
              UNAFPL_avg(j,k,icount)=mean(UNAFPL_all(j,k,*))
              VNAFPL_avg(j,k,icount)=mean(VNAFPL_all(j,k,*))

              CLONO2NAFPL_sig(j,k,icount)=stdev(CLONO2NAFPL_all(j,k,*))
              NOXNAFPL_sig(j,k,icount)=stdev(NOXNAFPL_all(j,k,*))
              NOYNAFPL_sig(j,k,icount)=stdev(NOYNAFPL_all(j,k,*))
              O3NAFPL_sig(j,k,icount)=stdev(O3NAFPL_all(j,k,*))
              OMEGANAFPL_sig(j,k,icount)=stdev(OMEGANAFPL_all(j,k,*))
              QSUMNAFPL_sig(j,k,icount)=stdev(QSUMNAFPL_all(j,k,*))
              TNAFPL_sig(j,k,icount)=stdev(TNAFPL_all(j,k,*))
              UNAFPL_sig(j,k,icount)=stdev(UNAFPL_all(j,k,*))
              VNAFPL_sig(j,k,icount)=stdev(VNAFPL_all(j,k,*))
          endfor
      endfor

      icount=icount+1L
goto,jump
;
; save yearly average and sigma file
;
saveit:
ofile=dirh+'AverageYear_VLH_06-22.sav'
save,file=ofile,lat,p,alt,CLONO2NAFPL_avg,NOXNAFPL_avg,NOYNAFPL_avg,O3NAFPL_avg,OMEGANAFPL_avg,$
     QSUMNAFPL_avg,TNAFPL_avg,UNAFPL_avg,VNAFPL_avg,CLONO2NAFPL_sig,NOXNAFPL_sig,NOYNAFPL_sig,$
     O3NAFPL_sig,OMEGANAFPL_sig,QSUMNAFPL_sig,TNAFPL_sig,UNAFPL_sig,VNAFPL_sig
end
