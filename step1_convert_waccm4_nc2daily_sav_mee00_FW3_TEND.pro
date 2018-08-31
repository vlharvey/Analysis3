;
; read WACCM4 netcdf data from Ethan Peck
; variables PTEND and TTEND
;
; /Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
;
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/'
;spawn,'ls '+dir+'mee00fpl_FW3NoSpin_paper1_Press_*TEND.sav',ifiles
ifiles=dir+['mee00fpl_FW3NoSpin_paper1_Press_PTTEND.sav','mee00fpl_FW3NoSpin_paper1_Press_T.sav']
nfile=n_elements(ifiles)
for i=0L,nfile-1L do begin
    restore,ifiles(i)
    print,ifiles(i)
endfor
DATE00FPL_all=DATE00FPL
PTTEND00FPL_all=PTTEND00FPL
T00FPL_all=T00FPL
;
; compute temperature tendency as day minus day-1
;
ndays=n_elements(DATE00FPL_all)
TTEND00FPL_all=0.*PTTEND00FPL_all
for i=1L,ndays-1L do TTEND00FPL_all(*,*,i)=(T00FPL_all(*,*,i)-T00FPL_all(*,*,i-1L))/86400.	; K/sec
TTEND00FPL_all(*,*,0)=TTEND00FPL_all(*,*,1)
;TTEND00FPL_all(*,*,ndays-1)=TTEND00FPL_all(*,*,ndays-2)

date=long(DATE00FPL_all)
nfiles=n_elements(DATE00FPL_all)
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
    DATE00FPL=sdate(ifile)
    PTTEND00FPL=reform(PTTEND00FPL_all(*,*,ifile))
    TTEND00FPL=reform(TTEND00FPL_all(*,*,ifile))
    ofile=dir+'mee00fpl_FW3.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_TEND.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATE00FPL,PTTEND00FPL,TTEND00FPL
endfor			; loop over days
end
