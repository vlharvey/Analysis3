;
; read WACCM3 netcdf data from Ethan Peck
; convert single file to daily files
;
; /Volumes/Data/WACCM/WACCM3/mee00fco
;
; variable names
; ALT             FLOAT     = Array[66]
; CLONO2          FLOAT     = Array[46, 66, 32487]
; DATES           LONG      = Array[32487]
; LAT             DOUBLE    = Array[46]
; NOX             FLOAT     = Array[46, 66, 32487]
; NOY             FLOAT     = Array[46, 66, 32487]
; O3              FLOAT     = Array[46, 66, 32487]
; OMEGA           FLOAT     = Array[46, 66, 32487]
; P               FLOAT     = Array[66]
; QSUM            FLOAT     = Array[46, 66, 32487]
; T               FLOAT     = Array[46, 66, 32487]
; U               FLOAT     = Array[46, 66, 32487]
; V               FLOAT     = Array[46, 66, 32487]
; VSTAR           FLOAT     = Array[46, 66, 1068]
; WSTAR           FLOAT     = Array[46, 66, 1068]
;
dir='/Volumes/Data/WACCM/WACCM3/mee00fco/'
restore,dir+'mee00_Press_vE.sav'
CLONO200FCO_all=CLONO2
DATE00FCO_all=DATES
NOX00FCO_all=NOX
NOY00FCO_all=NOY
O300FCO_all=O3
OMEGA00FCO_all=OMEGA
QSUM00FCO_all=QSUM
T00FCO_all=T
U00FCO_all=U
V00FCO_all=V
dates=long(DATE00FCO_all)
sdate=strcompress(dates,/remove_all)
nfiles=n_elements(DATE00FCO_all)
nl=n_elements(p)
nr=n_elements(LAT)
for ifile=0L,nfiles-1L do begin
    syear=strmid(sdate,0,4)
    smon=strmid(sdate,4,2)
    sday=strmid(sdate,6,2)
;
; strip out sday
;
    DATE00FCO=sdate(ifile)
    CLONO200FCO=reform(CLONO200FCO_all(*,*,ifile))
    NOX00FCO=reform(NOX00FCO_all(*,*,ifile))
    NOY00FCO=reform(NOY00FCO_all(*,*,ifile))
    O300FCO=reform(O300FCO_all(*,*,ifile))
    OMEGA00FCO=reform(OMEGA00FCO_all(*,*,ifile))
    QSUM00FCO=reform(QSUM00FCO_all(*,*,ifile))
    T00FCO=reform(T00FCO_all(*,*,ifile))
    U00FCO=reform(U00FCO_all(*,*,ifile))
    V00FCO=reform(V00FCO_all(*,*,ifile))
    ofile=dir+'mee00fco.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'.sav'
    print,ofile
    save,file=ofile,LAT,P,ALT,DATE00FCO,CLONO200FCO,NOX00FCO,NOY00FCO,O300FCO,$
         OMEGA00FCO,QSUM00FCO,T00FCO,U00FCO,V00FCO
endfor			; loop over days
end
