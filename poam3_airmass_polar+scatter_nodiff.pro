; 
; polarator projection with ozone and PV superimposed
;
@read_poam3_header
@read_poam3_airmass
@rd_winds_nc_th

SETPLOT='ps'
read,'setplot=',setplot
print,' '
ihem=1
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
loadct,38
icolmax=byte(!p.color)
icolmax=fix(icolmax)
if icolmax eq 0 then icolmax=255
mcolor=icolmax
device,decompose=0
nxdim=750
nydim=750
xorig=[0.15,0.55]
yorig=[0.35,0.35]
xlen=0.3
ylen=0.3
cbaryoff=0.03
cbarydel=0.02
!NOERAS=-1

thlev=reverse([3000.,2800.,2600.,2400.,2200.,2000.,1900.,1800.,$
          1700.,1600.,1500.,1450.,1400.,1350.,1300.,1250.,1200.,1150.,$
          1100.,1050.,1000.,950.,900.,850.,800.,750.,700.,650.,625.,$
      600.,575.,550.,525.,500.,475.,450.,425.,400.,375.,350.,325.,300.])
month=['Jan','Feb','Mar','Apr','May','Jun',$
       'Jul','Aug','Sep','Oct','Nov','Dec']
mon=['jan_','feb_','mar_','apr_','may_','jun_',$
       'jul_','aug_','sep_','oct_','nov_','dec_']
mday =[31,28,31,30,31,30,31,31,30,31,30,31]
thval=700.
;print,thlev
;read,'enter theta level ',thval
k=where(thval eq thlev)
k=k(0)
stheta=strcompress(string(thlev(k)),/remove_all)+'K'

if setplot ne 'ps' then begin
   lc=icolmax
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
nlvls=20
col1=5+indgen(nlvls)*mcolor/nlvls
dirw='/aura3/data/SLIMCAT_data/Datfiles/'
dirh='/aura2/harvey/Airmass/POAM3/'
hfile=[$
'ukmo_poam3_dec_2002_match_nodiff.airm']
kfile=n_elements(hfile)
for kk=0,kfile-1 do begin
close,11
openr,11,dirh+hfile(kk),/f77
read_poam3_header,11,charexp,ukmo,nmc,ecmwf,restart,rfile,istime,$
    dirw,nfiles,wfiles,dirs,sfiles,ictime,dtflow,dt,igw,dtout,ofile
dirw=strcompress(dirw,/remove_all)
ntraj=0L
noccul=30L*nfiles
for n=0,nfiles-1 do begin
;
; POAM 3 airmass
;
    READU,11,istime,ictime,time,ntraj
    if ntraj gt 0 then $
       read_poam3_airmass,11,ntraj,xn,yn,thn,agen,x0n,y0n,th0n,t0n,z0n,p0n,extn,$
          sadn,h2on,xno2n,o3n,extn_err,sadn_err,h2on_err,xno2n_err,o3n_err,pvn,$
          pn,msfn,zn,tmpn,qn,qdfn,markn,frday,frnght,xmint,minttime,vpn,sfn

    if n eq 0L then begin
       xsave=-9999.+fltarr(noccul,nfiles)
       ysave=-9999.+fltarr(noccul,nfiles)
       zsave=-9999.+fltarr(noccul,nfiles)
       x0save=-9999.+fltarr(noccul,nfiles)
       y0save=-9999.+fltarr(noccul,nfiles)
       z0save=-9999.+fltarr(noccul,nfiles)
       o3save=-9999.+fltarr(noccul,nfiles)
       agesave=-9999.+fltarr(noccul,nfiles)
       pvsave=-9999.+fltarr(noccul,nfiles)
       tmpsave=-9999.+fltarr(noccul,nfiles)
       mintsave=-9999.+fltarr(noccul,nfiles)
       marksave=-9999.+fltarr(noccul,nfiles)
       frdaysave=-9999.+fltarr(noccul,nfiles)
       stimesave=-9999L+lonarr(noccul,nfiles)
       ctimesave=-9999L+lonarr(noccul,nfiles)
       ctimes=-9999L+lonarr(nfiles)
       stimes=strarr(nfiles)
    endif
    ctimes(n)=ictime

; extract day, month, year information from trajectory file
    idy=strmid(string(ictime),8,2)
    imn=strmid(string(ictime),6,2)
    iyr=strmid(string(ictime),4,2)
    ihr=strmid(string(ictime),10,2)
    if strmid(iyr,0,1) eq '9' then $
       date=strcompress(string(FORMAT='(A3,A1,I2,A2,I4,A1,I2.2,A1)',$
       month(imn-1),' ',idy,', ',iyr+1900,' ',ihr,'Z'))
    if strmid(iyr,0,1) eq '0' then $
       date=strcompress(string(FORMAT='(A3,A1,I2,A2,I4,A1,I2.2,A1)',$
       month(imn-1),' ',idy,', ',iyr+2000,' ',ihr,'Z'))
;
;  read same day of UKMO isentropic data
;
    iflag=0
    stime=strcompress(string(ictime),/remove_all)
    ufile=dirw+'SLIMCAT_'+strmid(stime,0,8)+'_th.nc'
;   rd_winds_nc_th,ufile,nc,nr,nl,x,alat,th,pv2,prs2,msf2,u2,v2,q2,qdf2,iflag
;   if iflag eq 1 then goto,jumpday

; isolate UKMO in 2d on proper theta surface (ipv in PVU)
;   u=fltarr(nc,nr)
;   pv=fltarr(nc,nr)
;   y2d=fltarr(nc,nr)
;   x2d=fltarr(nc,nr)
;   for i=0,nc-1 do y2d(i,*)=alat
;   for j=0,nr-1 do x2d(*,j)=x
;   for j=0,nr-1 do begin
;       for i=0,nc-1 do begin
;           u(i,j)=u2(j,i,k)
;           pv(i,j)=pv2(j,i,k)
;       endfor
;   endfor
    stime=strcompress(string(ictime),/remove_all)
    stimes(n)=strcompress(string(ictime),/remove_all)

    smin=' ' & smax=' '
    if ntraj gt 0L then begin
        index=where(th0n eq thval and y0n gt 0.,ntraj)
        print,istime,ictime,time,ntraj
        if index(0) ne -1 then begin
           xn=xn(index)
           yn=yn(index)
           thn=thn(index)
           x0n=x0n(index)
           y0n=y0n(index)
           th0n=th0n(index)
           t0n=t0n(index)
           z0n=z0n(index)
           p0n=p0n(index)
           agen=agen(index)
           o3n=o3n(index)*1e6
           extn=extn(index)
           sadn=sadn(index)
           h2on=h2on(index)*1.e6
           xno2n=xno2n(index)*1.e9
           pvn=pvn(index)
           pn=pn(index)
           msfn=msfn(index)
           zn=zn(index)
           tmpn=tmpn(index)
           qn=qn(index)
           qdfn=qdfn(index)
           markn=markn(index)
           frday=frday(index)
           frnght=frnght(index)
           xmint=xmint(index)
           minttime=minttime(index)
           vpn=vpn(index)
           sfn=sfn(index)

           xsave(0:ntraj-1L,n)=xn
           ysave(0:ntraj-1L,n)=yn
           zsave(0:ntraj-1L,n)=thn
           x0save(0:ntraj-1L,n)=x0n
           y0save(0:ntraj-1L,n)=y0n
           z0save(0:ntraj-1L,n)=th0n
           agesave(0:ntraj-1L,n)=agen
           pvsave(0:ntraj-1L,n)=pvn
           o3save(0:ntraj-1L,n)=o3n
           tmpsave(0:ntraj-1L,n)=tmpn
           mintsave(0:ntraj-1L,n)=xmint
           marksave(0:ntraj-1L,n)=markn
           frdaysave(0:ntraj-1L,n)=frday*24.    ; total sunlit hours
           stimesave(0:ntraj-1L,n)=istime
           ctimesave(0:ntraj-1L,n)=ictime
        endif
    endif
    jumpday:
endfor          ; loop over days
xsave=xsave(0:ntraj-1L,*)
ysave=ysave(0:ntraj-1L,*)
zsave=zsave(0:ntraj-1L,*)
x0save=x0save(0:ntraj-1L,*)
y0save=y0save(0:ntraj-1L,*)
z0save=z0save(0:ntraj-1L,*)
agesave=agesave(0:ntraj-1L,*)
pvsave=pvsave(0:ntraj-1L,*)
o3save=o3save(0:ntraj-1L,*)
tmpsave=tmpsave(0:ntraj-1L,*)
mintsave=mintsave(0:ntraj-1L,*)
marksave=marksave(0:ntraj-1L,*)
frdaysave=frdaysave(0:ntraj-1L,*)
stimesave=stimesave(0:ntraj-1L,*)
ctimesave=ctimesave(0:ntraj-1L,*)

    result=size(agesave)
    nstep=result(2)
    for n=1,nstep-1 do begin
        erase
        !type=2^2+2^3
        erase
        xmn=xorig(0)
        xmx=xorig(0)+xlen
        ymn=yorig(0)
        ymx=yorig(0)+ylen
        set_viewport,xmn,xmx,ymn,ymx

        index=where(ctimesave(*,n) ne -9999)
        if index(0) eq -1 then goto,skipstep
        startdate=strcompress(string(min(ctimesave(index,n))),/remove_all)
        ofile='poam3_'+startdate+'_'+stheta
        if setplot eq 'ps' then begin
           lc=0
           set_plot,'ps'
           xsize=nxdim/100.
           ysize=nydim/100.
           !psym=0
           !p.font=0
           device,font_size=9
           device,/landscape,bits=8,filename=ofile+'+nodiff.ps'
           device,/color
           device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
                  xsize=xsize,ysize=ysize
        endif
        xyouts,.25,.9,'POAM III Trajectories + No Diffusion',/normal,charsize=2
        MAP_SET,90,0,0,/ortho,/noeras,charsize=1.5,$
                title='Valid '+startdate+'  ('+stheta+')'
        ufile=dirw+'SLIMCAT_'+strmid(startdate,0,8)+'_th.nc'
        rd_winds_nc_th,ufile,nc,nr,nl,alon,alat,th,pv2,prs2,msf2,u2,v2,q2,qdf2,iflag
        u=fltarr(nc+1,nr)
        v=fltarr(nc+1,nr)
        pv=fltarr(nc+1,nr)
        y2d=fltarr(nc+1,nr)
        x2d=fltarr(nc+1,nr)
        x=fltarr(nc+1)
        x(0:nc-1)=alon
        x(nc)=x(0)+360.
        for i=0,nc do y2d(i,*)=alat
        for j=0,nr-1 do x2d(*,j)=x
        u(0:nc-1,0:nr-1)=transpose(u2(*,*,k))
        v(0:nc-1,0:nr-1)=transpose(v2(*,*,k))
        pv(0:nc-1,0:nr-1)=transpose(pv2(*,*,k))
        u(nc,*)=u(0,*)
        v(nc,*)=v(0,*)
        pv(nc,*)=pv(0,*)
        s1=sqrt(u^2.+v^2.)
        index=where(y2d gt 0. and abs(pv) lt 10.)
        sfmin=min(pv(index))
        sfmax=max(pv(index))
        sfint=(sfmax-sfmin)/20.
        sfbin=sfmin+sfint*findgen(20)
        contour,pv,x,alat,/overplot,levels=sfbin,/noeras,color=lc,$
                c_labels=0*sfbin,max_value=9999.,c_charsize=2
        MAP_SET,90,0,0,/ortho,/noeras,/grid,/contin
        oplot,findgen(361),.1+0.*findgen(361),psym=0

        xpos=reform(xsave(*,0:n))
        ypos=reform(ysave(*,0:n))
        agepos=reform(agesave(*,0:n))
        pvpos=reform(pvsave(*,0:n))
        o3pos=reform(o3save(*,0:n))
        zpos=reform(zsave(*,0:n))
        x0pos=reform(x0save(*,0:n))
        y0pos=reform(y0save(*,0:n))
        index=where(xpos ne -9999. and agepos le 240.,nleg)
        xpos=xpos(index)
        ypos=ypos(index)
        x0pos=x0pos(index)
        y0pos=y0pos(index)
        agepos=agepos(index)
        pvpos=pvpos(index)
        o3pos=o3pos(index)
        o3min=2.0
        o3max=8.0
        oplot,x0pos,y0pos,psym=8,color=lc,symsize=2.5
        for ileg=0,nleg-1 do begin
            xp=x0pos(ileg) & yp=y0pos(ileg)
            oplot,[xp,xp],[yp,yp],psym=8,$
                  color=mcolor*(o3pos(ileg)-o3min)/(o3max-o3min),symsize=2
        
            oplot,[xpos(ileg),xpos(ileg)],[ypos(ileg),ypos(ileg)],psym=8,$
                  color=mcolor*(o3pos(ileg)-o3min)/(o3max-o3min),symsize=1.5
            index=where(x0pos eq xp and y0pos eq yp,ncon)

            if index(0) ne -1 then begin
               xlife=xpos(index)
               ylife=ypos(index)
               alife=agepos(index)
               o3life=o3pos(index)

               index=sort(alife)
               xlife=xlife(index)
               ylife=ylife(index)
               alife=alife(index)
               o3life=o3life(index)
               oplot,[xp,xlife(0)],[yp,ylife(0)],psym=0,thick=2,$
                      color=mcolor*(o3life(0)-o3min)/(o3max-o3min)
               for icon=1L,ncon-1L do begin
                   oplot,[xlife(icon-1),xlife(icon)],[ylife(icon-1),ylife(icon)],$
                         psym=0,thick=2,color=mcolor*(o3life(icon)-o3min)/(o3max-o3min)
               endfor
            endif
        endfor
      imin=o3min
      imax=o3max
      ymnb=ymn-cbaryoff
      ymxb=ymnb+cbarydel
      set_viewport,xmn,xmx,ymnb,ymxb
      !type=2^2+2^3+2^6
      plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],$
            xtitle='Ozone (ppmv)',charsize=1.5
      ybox=[0,10,10,0,0]
      x1=imin
      dx=(imax-imin)/float(nlvls)
      for j=0,nlvls-1 do begin
          xbox=[x1,x1,x1+dx,x1+dx,x1]
          polyfill,xbox,ybox,color=col1(j)
          x1=x1+dx
      endfor

      xmn=xorig(1)
      xmx=xorig(1)+xlen
      ymn=yorig(1)
      ymx=yorig(1)+ylen
      !type=2^2+2^3
      set_viewport,xmn,xmx,ymn,ymx
      index=where(pvpos gt 0. and pvpos lt 10.)
      pvmax=.1
      if index(0) ne -1 then pvmax=max(pvpos(index))+0.01
      plot,o3pos,pvpos,xrange=[0.,8.],yrange=[0.,pvmax],psym=8,symsize=0.5

        if setplot eq 'ps' then begin
           device, /close
           spawn,'convert '+ofile+'+nodiff.ps -rotate -90 '+ofile+'+nodiff.jpg'
        endif
skipstep:
    endfor
jumprun:
endfor		; loop over airmass runs
end
