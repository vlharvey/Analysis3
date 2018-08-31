;
; daily vertical profile of area occupied by cyclones vs anticyclones in each hemisphere
; on each day of the year save all 300 instances of the area profiles
;
loadct,39
mcolor=byte(!p.color)
icmm1=mcolor-1B
icmm2=mcolor-2B
device,decompose=0
a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill
!NOERAS=-1
SETPLOT='ps'
read,'setplot',setplot
nxdim=750
nydim=750
xorig=[0.15,0.55,0.15,0.55]
yorig=[0.55,0.55,0.15,0.15]
xlen=0.35
ylen=0.35
cbaryoff=0.1
cbarydel=0.01
if setplot ne 'ps' then begin
   !p.background=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif
dir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/3d_CO2x1SmidEmax_yBWCN_'
odir='/atmos/harvey/WACCM_data/Datfiles/Datfiles_Ethan_600yr/CO2x1SmidEmax_yBWCN/AREA_CO2x1SmidEmax_yBWCN_'
smonth=['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
nmonth=n_elements(smonth)

re=40000./2./!pi
earth_area=4.*!pi*re*re
hem_area=earth_area/2.0
rtd=double(180./!pi)
dtr=1./rtd
nrr=91L
yeq=findgen(nrr)
latcircle=fltarr(nrr)
hem_frac=fltarr(nrr)
for j=0,nrr-2 do begin
    hy=re*dtr
    dx=re*cos(yeq(j)*dtr)*360.*dtr
    latcircle(j)=dx*hy
endfor
for j=0,nrr-1 do begin
    if yeq(j) ge 0. then index=where(yeq ge yeq(j))
    if index(0) ne -1 then hem_frac(j)=100.*total(latcircle(index))/hem_area
    if yeq(j) eq 0. then hem_frac(j)=100.
endfor
;
; build MMDD dates
;
year1files=file_search(dir+'001????.nc3')
ndays=n_elements(year1files)
mmdd=strarr(ndays)
for ii=0L,ndays-1L do begin
    dum=strsplit(year1files(ii),'.',/extract)
    dum2=strsplit(dum(0),'_',/extract)
    mmdd(ii)=strmid(dum2(-1),3,4)
endfor
;
; loop over days of the year
;
for iday=280,ndays-1 do begin
    print,mmdd(iday)
    filenames=file_search(dir+'???'+mmdd(iday)+'.nc3')
    ofile=odir+mmdd(iday)+'.sav'
    dum=file_search(ofile)
    if dum(0) ne '' then goto,skipday
;
; loop over all years for this day (300 January 1sts)
;
    nfile=n_elements(filenames)
    if nfile ne 300 then stop,'Is there missing data for this day?'
    for ifile=0L,nfile-1L do begin
        ncfile0=filenames(ifile)
        dum=findfile(ncfile0)
;       print,dum
;       if dum(0) eq '' then goto,jump
        ncid=ncdf_open(ncfile0)
        result0=ncdf_inquire(ncid)
        for idim=0,result0.ndims-1 do begin
            ncdf_diminq,ncid,idim,name,dim
            if name eq 'number_of_latitudes' then nr=dim
            if name eq 'number_of_longitudes' then nc=dim
            if name eq 'number_of_levels' then nth=dim
;           print,'read ',name,' dimension ',dim
        endfor
        for ivar=0,result0.nvars-1 do begin
            result=ncdf_varinq(ncid,ivar)
            ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
            if result.name eq 'latitude' then alat=data
            if result.name eq 'longitude' then alon=data
            if result.name eq 'theta' then th=data
;           if result.name eq 'IPV' then ipv=data
;           if result.name eq 'P' then p=data
;           if result.name eq 'U' then u=data
;           if result.name eq 'V' then v=data
;           if result.name eq 'QDF' then qdf=data
;           if result.name eq 'CO' then co=data*1.e6
            if result.name eq 'GPH' then z=data/1000.
;           if result.name eq 'SF' then sf=data
            if result.name eq 'MARK' then mark2=data
;           print,ivar,result.name,min(data),max(data)
        endfor
        ncdf_close,ncid
        index=where(mark2 lt 0.)
        if index(0) ne -1L then mark2(index)=-1.
;
; calculate temperature
;
;       tp=0.*p
;       for k=0,nth-1 do tp(*,*,k) = th(k)*( (p(*,*,k)/1000.)^(.286) )

        if ifile eq 0L then begin
           dum=transpose(mark2(*,*,0))
           lon=0.*dum
           lat=0.*dum
           for i=0,nc-1 do lat(i,*)=alat
           for j=0,nr-1 do lon(*,j)=alon
           area=0.*lat
           deltax=alon(1)-alon(0)
           deltay=alat(1)-alat(0)
           for j=0,nr-1 do begin
               hy=re*deltay*dtr
               dx=re*cos(alat(j)*dtr)*deltax*dtr
               area(*,j)=dx*hy    ; area of each grid point
           endfor
           z_ztnh=fltarr(nfile,nth)
           z_ztsh=fltarr(nfile,nth)
           harea_ztnh=fltarr(nfile,nth)	; 300 January 1sts
           varea_ztnh=fltarr(nfile,nth)
           harea_ztsh=fltarr(nfile,nth)
           varea_ztsh=fltarr(nfile,nth)
        endif
;
; vortex areas in the polar cap
;
;erase
;map_set,90,0,0,/ortho,/contin,/grid,/noeras,title=mmdd(iday)+' '+strcompress(ifile,/r),color=0
        for thlev=0,nth-1 do begin
            mark1=transpose(mark2(*,*,thlev))
;
; NH
;
            index=where(lat ge 30. and mark1 gt 0.0,nn)
            if index(0) ne -1 then begin
;oplot,lon(index),lat(index),psym=1,color=0
               phem=100.*total(area(index))/hem_area      ; % of hemisphere
               for jj=1L,nrr-1L do begin
                   if hem_frac(jj) le phem and hem_frac(jj-1) gt phem then begin
                      scale=(hem_frac(jj)-phem)/(hem_frac(jj)-hem_frac(jj-1))
                      varea_ztnh(ifile,thlev)=yeq(jj)-scale*(yeq(jj)-yeq(jj-1)) ; Equivalent latitude
;print,hem_frac(jj-1),phem,hem_frac(jj),scale
;print,yeq(jj-1),varea_ztnh(ifile,thlev),yeq(jj)
                   endif
               endfor
            endif
            index=where(lat ge 30. and mark1 lt 0.0,nn)
            if index(0) ne -1 then begin
;oplot,lon(index),lat(index),psym=1,color=250
               phem=100.*total(area(index))/hem_area      ; % of hemisphere
               for jj=1L,nrr-1L do begin
                   if hem_frac(jj) le phem and hem_frac(jj-1) gt phem then begin
                      scale=(hem_frac(jj)-phem)/(hem_frac(jj)-hem_frac(jj-1))
                      harea_ztnh(ifile,thlev)=yeq(jj)-scale*(yeq(jj)-yeq(jj-1))        ; Equivalent latitude
                   endif
               endfor
            endif
;
; SH
;
            index=where(lat le -30. and mark1 gt 0.0,nn)
            if index(0) ne -1 then begin
               phem=100.*total(area(index))/hem_area      ; % of hemisphere
               for jj=1L,nrr-1L do begin
                   if hem_frac(jj) le phem and hem_frac(jj-1) gt phem then begin
                      scale=(hem_frac(jj)-phem)/(hem_frac(jj)-hem_frac(jj-1))
                      varea_ztsh(ifile,thlev)=yeq(jj)-scale*(yeq(jj)-yeq(jj-1)) ; Equivalent latitude
                   endif
               endfor
            endif
            index=where(lat le -30. and mark1 lt 0.0,nn)
            if index(0) ne -1 then begin
               phem=100.*total(area(index))/hem_area      ; % of hemisphere
               for jj=1L,nrr-1L do begin
                   if hem_frac(jj) le phem and hem_frac(jj-1) gt phem then begin
                      scale=(hem_frac(jj)-phem)/(hem_frac(jj)-hem_frac(jj-1))
                      harea_ztsh(ifile,thlev)=yeq(jj)-scale*(yeq(jj)-yeq(jj-1))        ; Equivalent latitude
                   endif
               endfor
            endif
        endfor	; loop over theta
;
; hemispheric average altitude
;
        dum=mean(z(nr/2:-1,*,*),dim=1)	; NH
        z_ztnh(ifile,*)=mean(dum,dim=1)
        dum=mean(z(0:nr/2-1,*,*),dim=1)	; SH
        z_ztsh(ifile,*)=mean(dum,dim=1)

    endfor	; loop over files
;
; save daily file containing areas for all years
;
    ofile=odir+mmdd(iday)+'.sav'
    print,'saving '+ofile
    save,filename=ofile,nth,th,z_ztnh,z_ztsh,harea_ztnh,varea_ztnh,harea_ztsh,varea_ztsh
;
; postscript file
;
    if setplot eq 'ps' then begin
       lc=0
       xsize=nxdim/100.
       ysize=nydim/100.
       set_plot,'ps'
       device,/landscape,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,xsize=xsize,ysize=ysize,$
              /bold,/color,bits_per_pixel=8,/helvetica,filename='Figures/area_prof_waccm_smidemax_'+mmdd(iday)+'.ps'
       !p.charsize=1.25
       !p.thick=2
       !p.charthick=2
       !y.thick=2
       !x.thick=2
    endif
;
; plot
;
    erase
    xyouts,.4,.95,mmdd(iday),/normal,charsize=2,charthick=2,color=0
    !type=2^2+2^3
    xmn=xorig(0)
    xmx=xorig(0)+xlen
    ymn=yorig(0)
    ymx=yorig(0)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    plot,varea_ztnh(0,*),z_ztnh(0,*),color=0,psym=2,title='NH Vortex',xrange=[0,90],yrange=[30,130]
    for i=0L,nfile-1L do oplot,varea_ztnh(i,*),z_ztnh(i,*),color=0,psym=2

    xmn=xorig(1)
    xmx=xorig(1)+xlen
    ymn=yorig(1)
    ymx=yorig(1)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    plot,harea_ztnh(0,*),z_ztnh(0,*),color=0,psym=2,title='NH Highs',xrange=[0,90],yrange=[30,130]
    for i=0L,nfile-1L do oplot,harea_ztnh(i,*),z_ztnh(i,*),color=250,psym=2

    xmn=xorig(2)
    xmx=xorig(2)+xlen
    ymn=yorig(2)
    ymx=yorig(2)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    plot,varea_ztsh(0,*),z_ztsh(0,*),color=0,psym=2,title='SH Vortex',xrange=[0,90],yrange=[30,130]
    for i=0L,nfile-1L do oplot,varea_ztsh(i,*),z_ztsh(i,*),color=0,psym=2

    xmn=xorig(3)
    xmx=xorig(3)+xlen
    ymn=yorig(3)
    ymx=yorig(3)+ylen
    set_viewport,xmn,xmx,ymn,ymx
    plot,harea_ztsh(0,*),z_ztsh(0,*),color=0,psym=2,title='SH Highs',xrange=[0,90],yrange=[30,130]
    for i=0L,nfile-1L do oplot,harea_ztsh(i,*),z_ztsh(i,*),color=250,psym=2
;
; Close PostScript file and return control to X-windows
;
    if setplot ne 'ps' then stop
    if setplot eq 'ps' then begin
       device, /close
       spawn,'convert -trim Figures/area_prof_waccm_smidemax_'+mmdd(iday)+'.ps -rotate -90 Figures/area_prof_waccm_smidemax_'+mmdd(iday)+'.jpg'
       spawn,'rm -f Figures/area_prof_waccm_smidemax_'+mmdd(iday)+'.ps'
    endif

skipday:
endfor	; loop over days of the year
end
