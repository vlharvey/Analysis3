;
; create WACCM4 noaur average year. Include averages and sigmas
;
@stddat
@kgmt
@ckday
@kdate

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
         PTTENDNAFPL_avg=fltarr(nr,nl,kday)
         TTENDNAFPL_avg=fltarr(nr,nl,kday)

         PTTENDNAFPL_sig=fltarr(nr,nl,kday)
         TTENDNAFPL_sig=fltarr(nr,nl,kday)
      endif
;
; declare new "all" arrays every day
;
      PTTENDNAFPL_all=fltarr(nr,nl,nyears)
      TTENDNAFPL_all=fltarr(nr,nl,nyears)
;
; loop over years and retain all data
;
      for iyear=0L,nyears-1L do begin
          restore,ifiles(iyear)
;         print,'restored '+ifiles(iyear)
          PTTENDNAFPL_all(*,*,iyear)=PTTENDNAFPL
          TTENDNAFPL_all(*,*,iyear)=TTENDNAFPL
      endfor
;
; fill yearly average and sigma arrays
;
      for j=0L,nr-1L do begin
          for k=0L,nl-1L do begin
              PTTENDNAFPL_avg(j,k,icount)=mean(PTTENDNAFPL_all(j,k,*))
              TTENDNAFPL_avg(j,k,icount)=mean(TTENDNAFPL_all(j,k,*))

              PTTENDNAFPL_sig(j,k,icount)=stdev(PTTENDNAFPL_all(j,k,*))
              TTENDNAFPL_sig(j,k,icount)=stdev(TTENDNAFPL_all(j,k,*))
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
save,file=ofile,lat,lev,PTTENDNAFPL_avg,TTENDNAFPL_avg,PTTENDNAFPL_sig,TTENDNAFPL_sig
end
