;
; read WACCM4 CO2x1SmidEmin_yBWCN set1 (and then set2) data and save daily files of 3d T, U, V, Z, CO
; NOTE: save Year150 from set1 and save Year151 from set2
;
; /atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set1
; /atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2
;
idir='/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set1/'
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmin_yBWCN/'

ivars=[$
'CO',$
'T',$
'U',$
'V',$
'Z3']
nvars=n_elements(ivars)
;
; loop over years in the first set
;
for iyear=1,150 do begin
    syear=string(format='(i4.4)',iyear)
;
; read WACCM year of 5 variables
; DATES           LONG      = Array[365]
; ILEV            DOUBLE    = Array[67]
; LAT             DOUBLE    = Array[96]
; LEV             DOUBLE    = Array[66]
; LON             DOUBLE    = Array[144]
; T3D             FLOAT     = Array[144, 96, 66, 365]
; U3D             FLOAT     = Array[144, 96, 66, 365]
; V3D             FLOAT     = Array[144, 96, 66, 365]
; Z33D            FLOAT     = Array[144, 96, 66, 365]
; CO3D            FLOAT     = Array[144, 96, 66, 365]
;
    for ii=0L,nvars-1L do restore,idir+ivars(ii)+'3d_CO2x1SmidEmin_yBWCN_'+syear+'_vE.sav'
    tp3d=t3d	; rename to avoid conflict with keyword
    lon=float(lon)
    lat=float(lat)
    lev=float(lev)
;
; loop over days and save
;
    sdates=string(format='(i7.7)',dates)
    nday=n_elements(dates)
    for iday=0L,nday-1L do begin
     
        cogrd=reform(CO3D(*,*,*,iday))
        tgrd=reform(TP3D(*,*,*,iday))
        ugrd=reform(U3D(*,*,*,iday))
        vgrd=reform(V3D(*,*,*,iday))
        zgrd=reform(Z33D(*,*,*,iday))

        print,'saved '+dir+'3d_CO2x1SmidEmin_yBWCN_'+sdates(iday)+'_vE.sav'
        save,file=dir+'3d_CO2x1SmidEmin_yBWCN_'+sdates(iday)+'_vE.sav',lon,lat,lev,cogrd,tgrd,ugrd,vgrd,zgrd

    endfor	; loop over days
endfor		; loop over years
;
; second set
;
idir='/atmos/pecked/WACCM/CO2x1SmidEmin_yBWCN/set2/'
;
; loop over years
;
for iyear=151,300 do begin
    syear=string(format='(i4.4)',iyear)
;
; read WACCM year of 5 variables
; DATES           LONG      = Array[365]
; ILEV            DOUBLE    = Array[67]
; LAT             DOUBLE    = Array[96]
; LEV             DOUBLE    = Array[66]
; LON             DOUBLE    = Array[144]
; T3D             FLOAT     = Array[144, 96, 66, 365]
; U3D             FLOAT     = Array[144, 96, 66, 365]
; V3D             FLOAT     = Array[144, 96, 66, 365]
; Z33D            FLOAT     = Array[144, 96, 66, 365]
; CO3D            FLOAT     = Array[144, 96, 66, 365]
;
    for ii=0L,nvars-1L do restore,idir+ivars(ii)+'3d_CO2x1SmidEmin_yBWCN_'+syear+'_vE.sav'
    tp3d=t3d    ; rename to avoid conflict with keyword
    lon=float(lon)
    lat=float(lat)
    lev=float(lev)
;
; loop over days and save
;
    sdates=string(format='(i7.7)',dates)
    nday=n_elements(dates)
    for iday=0L,nday-1L do begin

        cogrd=reform(CO3D(*,*,*,iday))
        tgrd=reform(TP3D(*,*,*,iday))
        ugrd=reform(U3D(*,*,*,iday))
        vgrd=reform(V3D(*,*,*,iday))
        zgrd=reform(Z33D(*,*,*,iday))

        print,'saved '+dir+'3d_CO2x1SmidEmin_yBWCN_'+sdates(iday)+'_vE.sav'
        save,file=dir+'3d_CO2x1SmidEmin_yBWCN_'+sdates(iday)+'_vE.sav',lon,lat,lev,cogrd,tgrd,ugrd,vgrd,zgrd

    endfor      ; loop over days
endfor          ; loop over years

end
