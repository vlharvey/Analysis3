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
      spawn,'ls '+dirh+'*_'+sdate+'_TEND.sav',ifiles
      nyears=n_elements(ifiles)
      print,nyears,' years on '+sdate
      restore,ifiles(0)
      nr=n_elements(lat)
      nl=n_elements(lev)
;
; declare yearly average and sigma arrays
;
      if icount eq 0L then begin
         PTTEND00FPL_avg=fltarr(nr,nl,kday)
         TTEND00FPL_avg=fltarr(nr,nl,kday)

         PTTEND00FPL_sig=fltarr(nr,nl,kday)
         TTEND00FPL_sig=fltarr(nr,nl,kday)
      endif
;
; declare new "all" arrays every day
;
      PTTEND00FPL_all=fltarr(nr,nl,nyears)
      TTEND00FPL_all=fltarr(nr,nl,nyears)
;
; loop over years and retain all data
;
      for iyear=0L,nyears-1L do begin
          restore,ifiles(iyear)
;         print,'restored '+ifiles(iyear)
          PTTEND00FPL_all(*,*,iyear)=PTTEND00FPL
          TTEND00FPL_all(*,*,iyear)=TTEND00FPL
      endfor
;
; fill yearly average and sigma arrays
;
      for j=0L,nr-1L do begin
          for k=0L,nl-1L do begin
              PTTEND00FPL_avg(j,k,icount)=mean(PTTEND00FPL_all(j,k,*))
              TTEND00FPL_avg(j,k,icount)=mean(TTEND00FPL_all(j,k,*))

              PTTEND00FPL_sig(j,k,icount)=stdev(PTTEND00FPL_all(j,k,*))
              TTEND00FPL_sig(j,k,icount)=stdev(TTEND00FPL_all(j,k,*))
          endfor
      endfor
jump2count:
      icount=icount+1L
goto,jump
;
; save yearly average and sigma file
;
saveit:
ofile=dirh+'AverageYear_VLH_TEND.sav'
print,ofile
save,file=ofile,lat,lev,PTTEND00FPL_avg,TTEND00FPL_avg,PTTEND00FPL_sig,TTEND00FPL_sig
end
