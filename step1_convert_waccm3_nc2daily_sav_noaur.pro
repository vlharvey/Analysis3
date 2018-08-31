;
; read WACCM3 netcdf data from Ethan Peck
; convert single file to daily files
;
; /Volumes/Data/WACCM/WACCM3/noaurfco
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
dir='/Volumes/Data/WACCM/WACCM3/noaurfco/'
restore,dir+'Fco_Press_vE.sav
CLONO2NAFCO_all=CLONO2
DATENAFCO_all=DATES
NOXNAFCO_all=NOX
NOYNAFCO_all=NOY
O3NAFCO_all=O3
OMEGANAFCO_all=OMEGA
QSUMNAFCO_all=QSUM
TNAFCO_all=T
UNAFCO_all=U
VNAFCO_all=V
dates=long(DATENAFCO_all)
sdate=strcompress(dates,/remove_all)
nfiles=n_elements(DATENAFCO_all)
nl=n_elements(p)
nr=n_elements(LAT)
for ifile=0L,nfiles-1L do begin
    syear=strmid(sdate,0,4)
    smon=strmid(sdate,4,2)
    sday=strmid(sdate,6,2)
;
; strip out sday
;
    DATENAFCO=sdate(ifile)
    CLONO2NAFCO=reform(CLONO2NAFCO_all(*,*,ifile))
    NOXNAFCO=reform(NOXNAFCO_all(*,*,ifile))
    NOYNAFCO=reform(NOYNAFCO_all(*,*,ifile))
    O3NAFCO=reform(O3NAFCO_all(*,*,ifile))
    OMEGANAFCO=reform(OMEGANAFCO_all(*,*,ifile))
    QSUMNAFCO=reform(QSUMNAFCO_all(*,*,ifile))
    TNAFCO=reform(TNAFCO_all(*,*,ifile))
    UNAFCO=reform(UNAFCO_all(*,*,ifile))
    VNAFCO=reform(VNAFCO_all(*,*,ifile))
    ofile=dir+'noaurfco.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'.sav'
    print,ofile
    save,file=ofile,LAT,P,ALT,DATENAFCO,CLONO2NAFCO,NOXNAFCO,NOYNAFCO,O3NAFCO,$
         OMEGANAFCO,QSUMNAFCO,TNAFCO,UNAFCO,VNAFCO
endfor			; loop over days
end
