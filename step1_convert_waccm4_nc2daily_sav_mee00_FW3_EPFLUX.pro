;
; read WACCM4 netcdf data from Ethan Peck
; variables: EPD FY FZ VRES
;
dir='/Volumes/Data/WACCM/WACCM4/mee00fpl_FW3/'

ifiles=dir+['mee00fpl_FW3NoSpin_paper1_Press_epd.sav','mee00fpl_FW3NoSpin_paper1_Press_fy.sav',$
            'mee00fpl_FW3NoSpin_paper1_Press_fz.sav','mee00fpl_FW3NoSpin_paper1_Press_vres.sav',$
            'mee00fpl_FW3NoSpin_paper1_Press_wres.sav']
nfile=n_elements(ifiles)
for i=0L,nfile-1L do begin
    restore,ifiles(i)
    print,ifiles(i)
endfor
DATE00FPL_all=DATE00FPL
FZ00FPL_all=FZ00FPL
VRES00FPL_all=VRES00FPL
EPD00FPL_all=EPD00FPL
FY00FPL_all=FY00FPL
WRES00FPL_all=WRES00FPL
;
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
    FZ00FPL=reform(FZ00FPL_all(*,*,ifile))
    VRES00FPL=reform(VRES00FPL_all(*,*,ifile))
    EPD00FPL=reform(EPD00FPL_all(*,*,ifile))
    FY00FPL=reform(FY00FPL_all(*,*,ifile))
    WRES00FPL=reform(WRES00FPL_all(*,*,ifile))
    ofile=dir+'mee00fpl_FW3.cam2.h3.Year'+syear(ifile)+'_'+smon(ifile)+sday(ifile)+'_EPFLUX.sav'
    print,ofile
    save,file=ofile,LAT,LEV,DATE00FPL,FZ00FPL,VRES00FPL,EPD00FPL,FY00FPL,WRES00FPL
endfor			; loop over days
end
