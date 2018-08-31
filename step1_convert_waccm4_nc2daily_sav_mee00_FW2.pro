;
; read WACCM4 netcdf data from Ethan Peck
; convert single file to daily files
;
; /Volumes/Data/WACCM/WACCM4/mee00fpl_FW2
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW2
;
; variable names for noaur run (meerun replace NA with 00) consistent with Ethans are : 
; ALT             FLOAT     = Array[66]		; globally averaged Z3
; CLONO200FPL     FLOAT     = Array[96, 66]
; DATE00FPL       FLOAT     = Array[2050]	; first 1 or 2 numbers is the year, next two are month, next two are day
; LAT             DOUBLE    = Array[96]
; LEV             DOUBLE    = Array[66]
; NOX00FPL        FLOAT     = Array[96, 66]
; NOY00FPL        FLOAT     = Array[96, 66]
; O300FPL         FLOAT     = Array[96, 66]
; OMEGA00FPL      FLOAT     = Array[96, 66]
; P               FLOAT     = Array[66]		; globally averaged pressure
; QSUM00FPL       FLOAT     = Array[96, 66]
; T00FPL          FLOAT     = Array[96, 66]
; U00FPL          FLOAT     = Array[96, 66]
; V00FPL          FLOAT     = Array[96, 66]
;
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW2/'
restore,dir+'mee00fpl_FW2NoSpin_Press.sav'
CLONO200FPL_all=CLONO200FPL
DATE00FPL_all=DATE00FPL
NOX00FPL_all=NOX00FPL
NOY00FPL_all=NOY00FPL
O300FPL_all=O300FPL
OMEGA00FPL_all=OMEGA00FPL
PRESS00FPL_all=PRESS00FPL
QSUM00FPL_all=QSUM00FPL
T00FPL_all=T00FPL
U00FPL_all=U00FPL
V00FPL_all=V00FPL
date=long(DATE00FPL_all)
nfiles=n_elements(DATE00FPL_all)
nl=n_elements(p)
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
    CLONO200FPL=reform(CLONO200FPL_all(*,*,ifile))
    NOX00FPL=reform(NOX00FPL_all(*,*,ifile))
    NOY00FPL=reform(NOY00FPL_all(*,*,ifile))
    O300FPL=reform(O300FPL_all(*,*,ifile))
    OMEGA00FPL=reform(OMEGA00FPL_all(*,*,ifile))
    PRESS00FPL=reform(PRESS00FPL_all(*,*,ifile))
    QSUM00FPL=reform(QSUM00FPL_all(*,*,ifile))
    T00FPL=reform(T00FPL_all(*,*,ifile))
    U00FPL=reform(U00FPL_all(*,*,ifile))
    V00FPL=reform(V00FPL_all(*,*,ifile))
    ofile=dir+'mee00fpl_FW2.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'.sav'
    print,ofile
    save,file=ofile,LAT,P,LEV,ALT,DATE00FPL,CLONO200FPL,NOX00FPL,NOY00FPL,O300FPL,$
         OMEGA00FPL,QSUM00FPL,T00FPL,U00FPL,V00FPL
endfor			; loop over days
end
