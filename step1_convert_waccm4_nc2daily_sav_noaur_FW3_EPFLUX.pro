;
; read WACCM4 netcdf data from Ethan Peck
; variables: EPD FY FZ VRES
;
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/
; /Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/
;
dir='/Volumes/Data/WACCM/WACCM4/noaurfpl_FW3/'

ifiles=dir+['noaurfpl_FW3NoSpin_paper1_Press_epd.sav','noaurfpl_FW3NoSpin_paper1_Press_fy.sav',$
            'noaurfpl_FW3NoSpin_paper1_Press_fz.sav','noaurfpl_FW3NoSpin_paper1_Press_vres.sav',$
            'noaurfpl_FW3NoSpin_paper1_Press_wres.sav']
nfile=n_elements(ifiles)
for i=0L,nfile-1L do begin
    restore,ifiles(i)
    print,ifiles(i)
endfor
DATENAFPL_all=DATENAFPL
FZNAFPL_all=FZNAFPL
VRESNAFPL_all=VRESNAFPL
EPDNAFPL_all=EPDNAFPL
FYNAFPL_all=FYNAFPL
WRESNAFPL_all=WRESNAFPL
;
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
    FZNAFPL=reform(FZNAFPL_all(*,*,ifile))
    VRESNAFPL=reform(VRESNAFPL_all(*,*,ifile))
    EPDNAFPL=reform(EPDNAFPL_all(*,*,ifile))
    FYNAFPL=reform(FYNAFPL_all(*,*,ifile))
    WRESNAFPL=reform(WRESNAFPL_all(*,*,ifile))
    ofile=dir+'noaurfpl_FW3.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_EPFLUX.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATENAFPL,FZNAFPL,VRESNAFPL,EPDNAFPL,FYNAFPL,WRESNAFPL
endfor			; loop over days
end
