;
; create WACCM4 mee00 average year. Include averages and sigmas
; Eliminate first 5 years
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
dirh='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW/mee00fpl_FW.cam2.h3.'
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
         CLONO200FPL_avg=fltarr(nr,nl,kday)
         NOX00FPL_avg=fltarr(nr,nl,kday)
         NOY00FPL_avg=fltarr(nr,nl,kday)
         O300FPL_avg=fltarr(nr,nl,kday)
         OMEGA00FPL_avg=fltarr(nr,nl,kday)
         QSUM00FPL_avg=fltarr(nr,nl,kday)
         T00FPL_avg=fltarr(nr,nl,kday)
         U00FPL_avg=fltarr(nr,nl,kday)
         V00FPL_avg=fltarr(nr,nl,kday)
         CLONO200FPL_sig=fltarr(nr,nl,kday)
         NOX00FPL_sig=fltarr(nr,nl,kday)
         NOY00FPL_sig=fltarr(nr,nl,kday)
         O300FPL_sig=fltarr(nr,nl,kday)
         OMEGA00FPL_sig=fltarr(nr,nl,kday)
         QSUM00FPL_sig=fltarr(nr,nl,kday)
         T00FPL_sig=fltarr(nr,nl,kday)
         U00FPL_sig=fltarr(nr,nl,kday)
         V00FPL_sig=fltarr(nr,nl,kday)
      endif
;
; declare new "all" arrays every day
;
      CLONO200FPL_all=fltarr(nr,nl,nyears)
      NOX00FPL_all=fltarr(nr,nl,nyears)
      NOY00FPL_all=fltarr(nr,nl,nyears)
      O300FPL_all=fltarr(nr,nl,nyears)
      OMEGA00FPL_all=fltarr(nr,nl,nyears)
      QSUM00FPL_all=fltarr(nr,nl,nyears)
      T00FPL_all=fltarr(nr,nl,nyears)
      U00FPL_all=fltarr(nr,nl,nyears)
      V00FPL_all=fltarr(nr,nl,nyears)
;
; loop over years and retain all data
;
      for iyear=0L,nyears-1L do begin
          restore,ifiles(iyear)
          print,'restored '+ifiles(iyear)
          CLONO200FPL_all(*,*,iyear)=CLONO200FPL
          NOX00FPL_all(*,*,iyear)=NOX00FPL
          NOY00FPL_all(*,*,iyear)=NOY00FPL
          O300FPL_all(*,*,iyear)=O300FPL
          OMEGA00FPL_all(*,*,iyear)=OMEGA00FPL
          QSUM00FPL_all(*,*,iyear)=QSUM00FPL
          T00FPL_all(*,*,iyear)=T00FPL
          U00FPL_all(*,*,iyear)=U00FPL
          V00FPL_all(*,*,iyear)=V00FPL
      endfor
;
; fill yearly average and sigma arrays
;
      for j=0L,nr-1L do begin
          for k=0L,nl-1L do begin
              CLONO200FPL_avg(j,k,icount)=mean(CLONO200FPL_all(j,k,*))
              NOX00FPL_avg(j,k,icount)=mean(NOX00FPL_all(j,k,*))
              NOY00FPL_avg(j,k,icount)=mean(NOY00FPL_all(j,k,*))
              O300FPL_avg(j,k,icount)=mean(O300FPL_all(j,k,*))
              OMEGA00FPL_avg(j,k,icount)=mean(OMEGA00FPL_all(j,k,*))
              QSUM00FPL_avg(j,k,icount)=mean(QSUM00FPL_all(j,k,*))
              T00FPL_avg(j,k,icount)=mean(T00FPL_all(j,k,*))
              U00FPL_avg(j,k,icount)=mean(U00FPL_all(j,k,*))
              V00FPL_avg(j,k,icount)=mean(V00FPL_all(j,k,*))

              CLONO200FPL_sig(j,k,icount)=stdev(CLONO200FPL_all(j,k,*))
              NOX00FPL_sig(j,k,icount)=stdev(NOX00FPL_all(j,k,*))
              NOY00FPL_sig(j,k,icount)=stdev(NOY00FPL_all(j,k,*))
              O300FPL_sig(j,k,icount)=stdev(O300FPL_all(j,k,*))
              OMEGA00FPL_sig(j,k,icount)=stdev(OMEGA00FPL_all(j,k,*))
              QSUM00FPL_sig(j,k,icount)=stdev(QSUM00FPL_all(j,k,*))
              T00FPL_sig(j,k,icount)=stdev(T00FPL_all(j,k,*))
              U00FPL_sig(j,k,icount)=stdev(U00FPL_all(j,k,*))
              V00FPL_sig(j,k,icount)=stdev(V00FPL_all(j,k,*))
          endfor
      endfor

      icount=icount+1L
goto,jump
;
; save yearly average and sigma file
;
saveit:
ofile=dirh+'AverageYear_VLH_06-22.sav'
save,file=ofile,lat,p,alt,CLONO200FPL_avg,NOX00FPL_avg,NOY00FPL_avg,O300FPL_avg,OMEGA00FPL_avg,$
     QSUM00FPL_avg,T00FPL_avg,U00FPL_avg,V00FPL_avg,CLONO200FPL_sig,NOX00FPL_sig,NOY00FPL_sig,$
     O300FPL_sig,OMEGA00FPL_sig,QSUM00FPL_sig,T00FPL_sig,U00FPL_sig,V00FPL_sig
end
