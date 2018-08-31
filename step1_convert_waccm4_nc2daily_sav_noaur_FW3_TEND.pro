;
; read WACCM4 netcdf data from Ethan Peck
; variables PTEND and TTEND
;
; /Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
;
dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/'
;spawn,'ls '+dir+'noaurfpl_FW3NoSpin_paper1_Press_*TEND.sav',ifiles
ifiles=dir+['noaurfpl_FW3NoSpin_paper1_Press_PTTEND.sav','noaurfpl_FW3NoSpin_paper1_Press_T.sav']
nfile=n_elements(ifiles)
for i=0L,nfile-1L do begin
    restore,ifiles(i)
    print,ifiles(i)
endfor
DATENAFPL_all=DATENAFPL
PTTENDNAFPL_all=PTTENDNAFPL
TNAFPL_all=TNAFPL
;
; compute temperature tendency as day minus day-1
;
ndays=n_elements(DATENAFPL_all)
TTENDNAFPL_all=0.*PTTENDNAFPL_all
for i=1L,ndays-1L do TTENDNAFPL_all(*,*,i)=(TNAFPL_all(*,*,i)-TNAFPL_all(*,*,i-1L))/86400.      ; K/sec
TTENDNAFPL_all(*,*,0)=TTENDNAFPL_all(*,*,1)
;TTENDNAFPL_all(*,*,ndays-1)=TTENDNAFPL_all(*,*,ndays-2)

date=long(DATENAFPL_all)
nfiles=n_elements(DATENAFPL_all)
nl=n_elements(lev)
nr=n_elements(LAT)
for ifile=0L,nfiles-1L do begin
    sdate=strcompress(date,/remove_all)
    index=where(date lt 100101)
    if index(0) ne -1L then sdate(index)='0'+sdate(index)
    syear=strmid(sdate,0,2)
    smon=strmid(sdate,2,2)
    sday=strmid(sdate,4,2)
;
; strip out sday
;
    DATENAFPL=sdate(ifile)
    PTTENDNAFPL=reform(PTTENDNAFPL_all(*,*,ifile))
    TTENDNAFPL=reform(TTENDNAFPL_all(*,*,ifile))
    ofile=dir+'noaurfpl_FW3.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_TEND.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATENAFPL,PTTENDNAFPL,TTENDNAFPL
endfor			; loop over days
end
