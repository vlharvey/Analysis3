;
; create WACCM3 mee00 average year. Include averages and sigmas
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
dirh='/Volumes/Data/WACCM/WACCM3/mee00fco/mee00fco.cam2.h3.'
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
         CLONO200FCO_avg=fltarr(nr,nl,kday)
         NOX00FCO_avg=fltarr(nr,nl,kday)
         NOY00FCO_avg=fltarr(nr,nl,kday)
         O300FCO_avg=fltarr(nr,nl,kday)
         OMEGA00FCO_avg=fltarr(nr,nl,kday)
         QSUM00FCO_avg=fltarr(nr,nl,kday)
         T00FCO_avg=fltarr(nr,nl,kday)
         U00FCO_avg=fltarr(nr,nl,kday)
         V00FCO_avg=fltarr(nr,nl,kday)
         CLONO200FCO_sig=fltarr(nr,nl,kday)
         NOX00FCO_sig=fltarr(nr,nl,kday)
         NOY00FCO_sig=fltarr(nr,nl,kday)
         O300FCO_sig=fltarr(nr,nl,kday)
         OMEGA00FCO_sig=fltarr(nr,nl,kday)
         QSUM00FCO_sig=fltarr(nr,nl,kday)
         T00FCO_sig=fltarr(nr,nl,kday)
         U00FCO_sig=fltarr(nr,nl,kday)
         V00FCO_sig=fltarr(nr,nl,kday)
      endif
;
; declare new "all" arrays every day
;
      CLONO200FCO_all=fltarr(nr,nl,nyears)
      NOX00FCO_all=fltarr(nr,nl,nyears)
      NOY00FCO_all=fltarr(nr,nl,nyears)
      O300FCO_all=fltarr(nr,nl,nyears)
      OMEGA00FCO_all=fltarr(nr,nl,nyears)
      QSUM00FCO_all=fltarr(nr,nl,nyears)
      T00FCO_all=fltarr(nr,nl,nyears)
      U00FCO_all=fltarr(nr,nl,nyears)
      V00FCO_all=fltarr(nr,nl,nyears)
;
; loop over years and retain all data
;
      for iyear=0L,nyears-1L do begin
          restore,ifiles(iyear)
;         print,'restored '+ifiles(iyear)
          CLONO200FCO_all(*,*,iyear)=CLONO200FCO
          NOX00FCO_all(*,*,iyear)=NOX00FCO
          NOY00FCO_all(*,*,iyear)=NOY00FCO
          O300FCO_all(*,*,iyear)=O300FCO
          OMEGA00FCO_all(*,*,iyear)=OMEGA00FCO
          QSUM00FCO_all(*,*,iyear)=QSUM00FCO
          T00FCO_all(*,*,iyear)=T00FCO
          U00FCO_all(*,*,iyear)=U00FCO
          V00FCO_all(*,*,iyear)=V00FCO
      endfor
;
; fill yearly average and sigma arrays
;
      for j=0L,nr-1L do begin
          for k=0L,nl-1L do begin
              CLONO200FCO_avg(j,k,icount)=mean(CLONO200FCO_all(j,k,*))
              NOX00FCO_avg(j,k,icount)=mean(NOX00FCO_all(j,k,*))
              NOY00FCO_avg(j,k,icount)=mean(NOY00FCO_all(j,k,*))
              O300FCO_avg(j,k,icount)=mean(O300FCO_all(j,k,*))
              OMEGA00FCO_avg(j,k,icount)=mean(OMEGA00FCO_all(j,k,*))
              QSUM00FCO_avg(j,k,icount)=mean(QSUM00FCO_all(j,k,*))
              T00FCO_avg(j,k,icount)=mean(T00FCO_all(j,k,*))
              U00FCO_avg(j,k,icount)=mean(U00FCO_all(j,k,*))
              V00FCO_avg(j,k,icount)=mean(V00FCO_all(j,k,*))

              CLONO200FCO_sig(j,k,icount)=stdev(CLONO200FCO_all(j,k,*))
              NOX00FCO_sig(j,k,icount)=stdev(NOX00FCO_all(j,k,*))
              NOY00FCO_sig(j,k,icount)=stdev(NOY00FCO_all(j,k,*))
              O300FCO_sig(j,k,icount)=stdev(O300FCO_all(j,k,*))
              OMEGA00FCO_sig(j,k,icount)=stdev(OMEGA00FCO_all(j,k,*))
              QSUM00FCO_sig(j,k,icount)=stdev(QSUM00FCO_all(j,k,*))
              T00FCO_sig(j,k,icount)=stdev(T00FCO_all(j,k,*))
              U00FCO_sig(j,k,icount)=stdev(U00FCO_all(j,k,*))
              V00FCO_sig(j,k,icount)=stdev(V00FCO_all(j,k,*))
          endfor
      endfor
jump2count:
      icount=icount+1L
goto,jump
;
; save yearly average and sigma file
;
saveit:
ofile=dirh+'AverageYear_VLH.sav'
print,ofile
save,file=ofile,lat,p,alt,CLONO200FCO_avg,NOX00FCO_avg,NOY00FCO_avg,O300FCO_avg,OMEGA00FCO_avg,$
     QSUM00FCO_avg,T00FCO_avg,U00FCO_avg,V00FCO_avg,CLONO200FCO_sig,NOX00FCO_sig,NOY00FCO_sig,$
     O300FCO_sig,OMEGA00FCO_sig,QSUM00FCO_sig,T00FCO_sig,U00FCO_sig,V00FCO_sig
end
