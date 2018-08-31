;*******************************************************************************
;  Program adapted from : ace_merged+soundings_v1.pro
;  adapted from merge_ace_theta_data+soundings.pro
;                                                                       
;  reads SOCRATES measurement locations database and creates merged isentropic and sounding database
;                                      
;  Input data:	SOCRATES SD-WACCM SOSST data
;  Input files: /Volumes/cloud/data/WACCM_Alpha/Datfiles_SOSST/dmps_socrates.sdwaccm.2022 (has pv, p, t, z, h2o and o3)
;  Output data:	SOCRATES NOGAPS merged and sounding data
;
;  Programed by: V. Lynn Harvey  Sep 2015  LASP
;
; IDL> restore,'dmps_socrates.sdwaccm.2023
; DATE            LONG      = Array[5023]
; DYNTROP         FLOAT     = Array[5023]
; H2O_PROF        FLOAT     = Array[5023, 13]		; already in ppmv
; ID              INT       = Array[5023]
; LATITUDE        FLOAT     = Array[5023]
; LONGITUDE       FLOAT     = Array[5023]
; O3_PROF         FLOAT     = Array[5023, 13]		; already in ppmv
; PTHERMTROP      FLOAT     = Array[5023]
; PV_PROF         FLOAT     = Array[5023, 13]
; P_PROF          FLOAT     = Array[5023, 13]
; TH              FLOAT     = Array[13]
; THTHERMTROP     FLOAT     = Array[5023]
; TIME            FLOAT     = Array[5023]
; TP_PROF         FLOAT     = Array[5023, 13]
; ZTHERMTROP      FLOAT     = Array[5023]
; Z_PROF          FLOAT     = Array[5023, 13]
;
; Isentropic data OUTPUT:                                                      *
;       => number of occultations                                              *
;       => time, latitude, longitude, direction to satellite                   *
;               pressure on theta, height of theta (km),                       *
;               pressure of tropopause, height of tropopause (km),             *
;               potential temperature of tropopause, cloud flag, mode          *
;       => O3, H2O
;                                                                              *
; Sounding OUTPUT:                                                             *
;       => number of occultations                                              *
;       => time, latitude, longitude, direction to satellite                   *
;               pressure of tropopause, height of tropopause (km),             *
;               potential temperature of tropopause, sunrise/sunset mode       *
;       => number of levels                                                    *
;       => vertical profiles of either ozone or extinction, then pressure,     *
;       => potential temperature, altitude, cloud flag, and error              *
;
;*******************************************************************************
@interp_ace
@tropopause
month=['jan_','feb_','mar_','apr_','may_','jun_',$
       'jul_','aug_','sep_','oct_','nov_','dec_']
smon=['01','02','03','04','05','06','07','08','09','10','11','12']
mday=[31,28,31,30,31,30,31,31,30,31,30,31]
nmonth=n_elements(mday)
odir='/Volumes/cloud/data/WACCM_data/Merged_data/'
sdir='/Volumes/cloud/data/WACCM_data/Sound_data/'
ddir='/Volumes/cloud/data/WACCM_data/Datfiles_SOSST/'
syear=['2023']		;'2022','2023','2024']	; dates in here are all 2013. only use syear to open the file, else use syear0
syear0=['2013']
nyear=n_elements(syear)
;
; loop over years
;
for iyear=0,nyear-1 do begin
;
; restore data
;
    restore,ddir+'dmps_socrates.sdwaccm.'+syear(iyear)
    ntheta=n_elements(th)
    ace_x=LONGITUDE
    ace_y=LATITUDE
    ace_d=-99.+0.*ace_y
    ace_t=TIME
;
; loop over months
;
    for imonth=0L,nmonth-1L do begin
;
; loop over days
;
        leapyr=(fix(syear0(iyear)) mod 4)
        if leapyr eq 0 then mday(1)=29
        if leapyr ne 0 then mday(1)=28
        for iday=1,mday(fix(smon(imonth))-1) do begin
            sday=string(iday,format='(i2.2)')
            yymmdd=long(syear0(iyear)+smon(imonth)+sday)
            norbit=0L & morbit=0L
            today=where(date eq yymmdd,norbit)
            if today(0) eq -1 then goto,no_data		; no data today
            if norbit lt 2L then goto,no_data		; if only 1 profile then skip for now
;
; isentropic arrays
;
            tace=fltarr(ntheta,norbit)
            xace=fltarr(ntheta,norbit)
            yace=fltarr(ntheta,norbit)
            dace=fltarr(ntheta,norbit)
            pace=transpose(p_prof(today,*))
            zace=transpose(z_prof(today,*))
            thace=fltarr(ntheta,norbit)
            tropz=fltarr(ntheta,norbit)
            tropp=fltarr(ntheta,norbit)
            tropth=fltarr(ntheta,norbit)
            cloud=-99.+fltarr(ntheta,norbit)
            mode=lonarr(ntheta,norbit)
            smode=strarr(ntheta,norbit)
            o3ace=transpose(o3_prof(today,*))
            h2oace=transpose(h2o_prof(today,*))
;
; todays soundings
;
            t_sound=ace_t(today)
            for ii=0L,ntheta-1L do tace(ii,*)=t_sound	; isentropic arrays that do not vary with altitude
            x_sound=ace_x(today)
            for ii=0L,ntheta-1L do xace(ii,*)=x_sound
            y_sound=ace_y(today)
            for ii=0L,ntheta-1L do yace(ii,*)=y_sound
            d_sound=ace_d(today)
            for ii=0L,ntheta-1L do dace(ii,*)=d_sound

            for ii=0L,ntheta-1L do tropz(ii,*)=ZTHERMTROP(today)
            for ii=0L,ntheta-1L do tropp(ii,*)=PTHERMTROP(today)
            for ii=0L,ntheta-1L do tropth(ii,*)=THTHERMTROP(today)
            for ii=0L,ntheta-1L do mode(ii,*)=id(today)
            for ii=0L,ntheta-1L do smode(ii,*)=' '
            for jj=0L,norbit-1L do thace(*,jj)=th

            press_sound=p_prof(today,*)
            temp_sound=tp_prof(today,*)
            theta_sound=0.*temp_sound
            for jj=0L,norbit-1L do theta_sound(jj,*)=th
            z_sound=z_prof(today,*)
            cloud_sound=-99.+0.*press_sound
            o3_sound=o3_prof(today,*)
            h2o_sound=h2o_prof(today,*)

            tsave=reform(tace(ntheta-1,*))
            xsave=reform(xace(ntheta-1,*))
            ysave=reform(yace(ntheta-1,*))
            dsave=reform(dace(ntheta-1,*))
            psave=reform(pace(ntheta-1,*))
            zsave=reform(zace(ntheta-1,*))
            tropzsave=reform(tropz(ntheta-1,*))
            troppsave=reform(tropp(ntheta-1,*))
            tropthsave=reform(tropth(ntheta-1,*))
            cloudsave=reform(cloud(ntheta-1,*))
            modesave=reform(mode(ntheta-1,*))
;
; write forward merged data
;
             index=sort(tace)
             tace=tace(index)
             xace=xace(index)
             yace=yace(index)
             dace=dace(index)
             pace=pace(index)
             zace=zace(index)
             thace=thace(index)
             tropz=tropz(index)
             tropp=tropp(index)
             tropth=tropth(index)
             cloud=cloud(index)
             mode=mode(index)
             o3ace=o3ace(index)
             h2oace=h2oace(index)
             no2dat=0.
             no2errdat=0.
             o3_errdat=0.
             h2o_errdat=0.
;
; TRUNCATE PROFILES AT 1000 K TO CONSERVE SPACE         VLH 8/16/11
;
             good=where(thace le 1000.,morbit)
             tace=tace(good)
             xace=xace(good)
             yace=yace(good)
             dace=dace(good)
             pace=pace(good)
             zace=zace(good)
             thace=thace(good)
             tropz=tropz(good)
             tropp=tropp(good)
             tropth=tropth(good)
             cloud=cloud(good)
             mode=mode(good)
             o3ace=o3ace(good)
             h2oace=h2oace(good)
;
; write forward merged data
;
             ofile='socrates_'+month(fix(smon(imonth))-1)+sday+'_'+syear0(iyear)+'.merged'
             close,20
             openw,20,odir+ofile
             printf,20,yymmdd
             printf,20,morbit
             for i=0L,morbit-1L do begin
                 t=tace(i)
                 x=xace(i)
                 y=yace(i)
                 d=dace(i)
                 p=pace(i)
                 z=zace(i)
                 th0=thace(i)
                 z_trop=tropz(i)
                 p_trop=tropp(i)
                 th_trop=tropth(i)
                 cl=cloud(i)
                 m=mode(i)
                 o3dat= o3ace(i)
                 h2odat=h2oace(i)
                 printf,20,t,y,x,th0,d,p,z,p_trop,z_trop,th_trop,cl,m
                 printf,20,no2dat,o3dat,h2odat
                 printf,20,no2errdat,o3_errdat,h2o_errdat
             endfor
;
; write backward merged data
;
             index=reverse(sort(tace))
             tace=tace(index)
             xace=xace(index)
             yace=yace(index)
             dace=dace(index)
             pace=pace(index)
             zace=zace(index)
             thace=thace(index)
             tropz=tropz(index)
             tropp=tropp(index)
             tropth=tropth(index)
             cloud=cloud(index)
             mode=mode(index)
             o3ace=o3ace(index)
             h2oace=h2oace(index)
             no2dat=0.
             no2errace=0.
             o3errace=0.
             h2oerrace=0.
             morbit=n_elements(tace)
             ofile='socrates_'+month(fix(smon(imonth))-1)+sday+'_'+syear0(iyear)+'.merged.back'
             close,20
             openw,20,odir+ofile
             printf,20,yymmdd
             printf,20,morbit
             for i=0L,morbit-1L do begin
                 t=tace(i)
                 x=xace(i)
                 y=yace(i)
                 d=dace(i)
                 p=pace(i)
                 z=zace(i)
                 th0=thace(i)
                 z_trop=tropz(i)
                 p_trop=tropp(i)
                 th_trop=tropth(i)
                 cl=cloud(i)
                 m=mode(i)
                 o3dat=o3ace(i)
                 h2odat=h2oace(i)
                 printf,20,t,y,x,th0,d,p,z,p_trop,z_trop,th_trop,cl,m
                 printf,20,no2dat,o3dat,h2odat
                 printf,20,no2errdat,o3_errdat,h2o_errdat
             endfor

; WRITE SOUNDING OUTPUT
             time=t_sound
             index=sort(time)
             press_sound=press_sound(index,*)
             temp_sound=temp_sound(index,*)
             theta_sound=theta_sound(index,*)
             cloud_sound=cloud_sound(index,*)
             o3_sound=o3_sound(index,*)
             h2o_sound=h2o_sound(index,*)
             o3err_sound=0.*o3_sound
             h2oerr_sound=0.*h2o_sound
             o3file='socrates_'+month(fix(smon(imonth))-1)+sday+'_'+syear0(iyear)+'_o3.sound'
             h2ofile='socrates_'+month(fix(smon(imonth))-1)+sday+'_'+syear0(iyear)+'_h2o.sound'
             close,21,25
             openw,21,sdir+o3file
             openw,25,sdir+h2ofile
             printf,21,norbit
             printf,25,norbit
             for i=0L,norbit-1L do begin
                 t=tsave(i)
                 x=xsave(i)
                 y=ysave(i)
                 d=dsave(i)
                 p=psave(i)
                 z=zsave(i)
                 z_trop=tropzsave(i)
                 p_trop=troppsave(i)
                 th_trop=tropthsave(i)
                 cl=cloudsave(i)
                 m=modesave(i)
                 printf,21,t,y,x,d,p_trop,z_trop,th_trop,m
                 printf,25,t,y,x,d,p_trop,z_trop,th_trop,m
;
; TRUNCATE PROFILES AT 1000 K TO CONSERVE SPACE         VLH 8/21/02
;
                 if i eq 0 then kindex=where(reform(theta_sound(i,*)) le 1000.,no3)

                 printf,21,no3
                 printf,25,no3
                 dum=reform(o3_sound(i,kindex))
                 printf,21,dum		; 21=ozone
                 dum=reform(h2o_sound(i,kindex))
                 printf,25,dum		; 25=water vapor
; OZONE
                 p_sound_o3=reform(press_sound(i,kindex))
                 th_sound_o3=reform(theta_sound(i,kindex))
                 z_sound_o3=reform(z_sound(i,kindex))
                 cl_sound_o3=-99.+0.*reform(press_sound(i,kindex))
                 printf,21,p_sound_o3
                 printf,21,th_sound_o3
                 printf,21,z_sound_o3
                 printf,21,cl_sound_o3
                 dum=reform(o3err_sound(i,kindex))
                 printf,21,dum
; H2O
                 p_sound_h2o=reform(press_sound(i,kindex))
                 th_sound_h2o=reform(theta_sound(i,kindex))
                 z_sound_h2o=reform(z_sound(i,kindex))
                 cl_sound_h2o=-99.+0.*reform(press_sound(i,kindex))
                 printf,25,p_sound_h2o
                 printf,25,th_sound_h2o
                 printf,25,z_sound_h2o
                 printf,25,cl_sound_h2o
                 dum=reform(h2oerr_sound(i,kindex))
                 printf,25,dum
             endfor				; end of loop over orbits
             close,19,20,21,25
             goto,end_day_data

; if no data
             no_data:
             if today(0) eq -1L or norbit lt 2L then begin
                close,19,20,21,25
                ofile1='socrates_'+month(fix(smon(imonth))-1)+sday+'_'+syear0(iyear)+'.merged'
                ofile2='socrates_'+month(fix(smon(imonth))-1)+sday+'_'+syear0(iyear)+'.merged.back'
                openw,19,odir+ofile1
                printf,19,yymmdd
                printf,19,morbit
                openw,20,odir+ofile2
                printf,20,yymmdd
                printf,20,morbit
 
                norbit=0L	; comment this out when code can deal with 1 profile/day
                o3file='socrates_'+month(fix(smon(imonth))-1)+sday+'_'+syear0(iyear)+'_o3.sound'
                h2ofile='socrates_'+month(fix(smon(imonth))-1)+sday+'_'+syear0(iyear)+'_h2o.sound'
                openw,21,sdir+o3file
                openw,25,sdir+h2ofile
                printf,21,norbit
                printf,25,norbit
                close,19,20,21,25
             endif
             end_day_data:
if norbit gt 0L then begin
index=where(o3_sound gt 0.)
print,syear0(iyear)+smon(imonth)+sday+' ',norbit,morbit,min(o3_sound(index)),max(o3_sound)
endif
if norbit eq 0L then print,syear0(iyear)+smon(imonth)+sday+' ',norbit
        endfor			; loop over days
    endfor			; loop over months
endfor				; loop over years
end
