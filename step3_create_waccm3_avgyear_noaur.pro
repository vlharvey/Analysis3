;
; create WACCM3 noaur average year. Include averages and sigmas
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
dirh='/Volumes/Data/WACCM/WACCM3/noaurfco/noaurfco.cam2.h3.'
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
      spawn,'ls '+dirh+'*_'+sdate+'.sav',ifiles
      nyears=n_elements(ifiles)
      print,nyears,' years on '+sdate
      restore,ifiles(0)
      nr=n_elements(lat)
      nl=n_elements(p)
;
; declare yearly average and sigma arrays
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
         CLONO2NAFCO_sig=fltarr(nr,nl,kday)
         NOXNAFCO_sig=fltarr(nr,nl,kday)
         NOYNAFCO_sig=fltarr(nr,nl,kday)
         O3NAFCO_sig=fltarr(nr,nl,kday)
         OMEGANAFCO_sig=fltarr(nr,nl,kday)
         QSUMNAFCO_sig=fltarr(nr,nl,kday)
         TNAFCO_sig=fltarr(nr,nl,kday)
         UNAFCO_sig=fltarr(nr,nl,kday)
         VNAFCO_sig=fltarr(nr,nl,kday)
      endif
;
; declare new "all" arrays every day
;
      CLONO2NAFCO_all=fltarr(nr,nl,nyears)
      NOXNAFCO_all=fltarr(nr,nl,nyears)
      NOYNAFCO_all=fltarr(nr,nl,nyears)
      O3NAFCO_all=fltarr(nr,nl,nyears)
      OMEGANAFCO_all=fltarr(nr,nl,nyears)
      QSUMNAFCO_all=fltarr(nr,nl,nyears)
      TNAFCO_all=fltarr(nr,nl,nyears)
      UNAFCO_all=fltarr(nr,nl,nyears)
      VNAFCO_all=fltarr(nr,nl,nyears)
;
; loop over years and retain all data
;
      for iyear=0L,nyears-1L do begin
          restore,ifiles(iyear)
;         print,'restored '+ifiles(iyear)
          CLONO2NAFCO_all(*,*,iyear)=CLONO2NAFCO
          NOXNAFCO_all(*,*,iyear)=NOXNAFCO
          NOYNAFCO_all(*,*,iyear)=NOYNAFCO
          O3NAFCO_all(*,*,iyear)=O3NAFCO
          OMEGANAFCO_all(*,*,iyear)=OMEGANAFCO
          QSUMNAFCO_all(*,*,iyear)=QSUMNAFCO
          TNAFCO_all(*,*,iyear)=TNAFCO
          UNAFCO_all(*,*,iyear)=UNAFCO
          VNAFCO_all(*,*,iyear)=VNAFCO
      endfor
;
; fill yearly average and sigma arrays
;
      for j=0L,nr-1L do begin
          for k=0L,nl-1L do begin
              CLONO2NAFCO_avg(j,k,icount)=mean(CLONO2NAFCO_all(j,k,*))
              NOXNAFCO_avg(j,k,icount)=mean(NOXNAFCO_all(j,k,*))
              NOYNAFCO_avg(j,k,icount)=mean(NOYNAFCO_all(j,k,*))
              O3NAFCO_avg(j,k,icount)=mean(O3NAFCO_all(j,k,*))
              OMEGANAFCO_avg(j,k,icount)=mean(OMEGANAFCO_all(j,k,*))
              QSUMNAFCO_avg(j,k,icount)=mean(QSUMNAFCO_all(j,k,*))
              TNAFCO_avg(j,k,icount)=mean(TNAFCO_all(j,k,*))
              UNAFCO_avg(j,k,icount)=mean(UNAFCO_all(j,k,*))
              VNAFCO_avg(j,k,icount)=mean(VNAFCO_all(j,k,*))

              CLONO2NAFCO_sig(j,k,icount)=stdev(CLONO2NAFCO_all(j,k,*))
              NOXNAFCO_sig(j,k,icount)=stdev(NOXNAFCO_all(j,k,*))
              NOYNAFCO_sig(j,k,icount)=stdev(NOYNAFCO_all(j,k,*))
              O3NAFCO_sig(j,k,icount)=stdev(O3NAFCO_all(j,k,*))
              OMEGANAFCO_sig(j,k,icount)=stdev(OMEGANAFCO_all(j,k,*))
              QSUMNAFCO_sig(j,k,icount)=stdev(QSUMNAFCO_all(j,k,*))
              TNAFCO_sig(j,k,icount)=stdev(TNAFCO_all(j,k,*))
              UNAFCO_sig(j,k,icount)=stdev(UNAFCO_all(j,k,*))
              VNAFCO_sig(j,k,icount)=stdev(VNAFCO_all(j,k,*))
          endfor
      endfor

      icount=icount+1L
goto,jump
;
; save yearly average and sigma file
;
saveit:
ofile=dirh+'AverageYear_VLH.sav'
print,ofile
save,file=ofile,lat,p,alt,CLONO2NAFCO_avg,NOXNAFCO_avg,NOYNAFCO_avg,O3NAFCO_avg,OMEGANAFCO_avg,$
     QSUMNAFCO_avg,TNAFCO_avg,UNAFCO_avg,VNAFCO_avg,CLONO2NAFCO_sig,NOXNAFCO_sig,NOYNAFCO_sig,$
     O3NAFCO_sig,OMEGANAFCO_sig,QSUMNAFCO_sig,TNAFCO_sig,UNAFCO_sig,VNAFCO_sig
end
