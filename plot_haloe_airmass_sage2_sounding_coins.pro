
; Read coincidences between HALOE airmasses and SAGE II soundings
; and apply KS test statistic

@ks_stats

a=findgen(8)*(2*!pi/8.)
usersym,cos(a),sin(a),/fill

; set color table
loadct,38
mcolor=byte(!p.color)
mcolor=fix(mcolor)
if mcolor eq 0 then mcolor=255
icmm1=mcolor-1
icmm2=mcolor-2

; define viewport location
nxdim=750
nydim=750
xorig=[0.20,0.20]
yorig=[0.65,0.15]
xlen=0.6
ylen=0.3
cbaryoff=0.10
cbarydel=0.02

SETPLOT='x'
;read,'setplot?',setplot

if setplot ne 'ps' then begin
   lc=mcolor
   window,4,xsize=nxdim,ysize=nydim,retain=2,colors=162
endif

dirs='/aura3/data/SAGE_II_data/Sound_data/sage2_'
diri='/aura2/harvey/Airmass/Validation/Datfiles/sage2_'

dum=strarr(1)
month=['jan_','feb_','mar_','apr_','may_','jun_',$
       'jul_','aug_','sep_','oct_','nov_','dec_']
mlab=['jan','feb','mar','apr','may','jun',$
       'jul','aug','sep','oct','nov','dec']
mday =[31,28,31,30,31,30,31,31,30,31,30,31]
thlab=['330k','340k','350k','360k','370k',$
       '380k','390k','400k','425k','450k','475k','500k',$
       '525k','550k','600k','700k','800k','900k','1000k',$
       '1200k','1400k','1600k','1800k','2000k']
thlev=[330.,340.,350.,360.,370.,380.,390.,400.,425.,450.,475.,$
       500.,525.,550.,600.,700.,800.,900.,1000.,1200.,1400.,$
       1600.,1800.,2000.]
nth=n_elements(thlab)
norbit=50

mday=[31,28,31,30,31,30,31,31,30,31,30,31]
year=[1992,1993,1994,1995,1996,1997,1998,1999,2000,2001]
nyear=n_elements(year)

for iy=0,nyear-1 do begin
    lstyr=year(iy)
    ledyr=year(iy)
    for im=0,12-1 do begin
        lstmn=im+1
        ledmn=im+1
        mday=[31,28,31,30,31,30,31,31,30,31,30,31]
        if year(iy) mod 4 eq 0 then mday(1)=29
        ndim=mday(im)*nth
        tcoin_save=9999.+fltarr(ndim)
        xcoin_save=9999.+fltarr(ndim)
        ycoin_save=9999.+fltarr(ndim)
        thcoin_save=9999.+fltarr(ndim)
        x0coin_save=9999.+fltarr(ndim)
        y0coin_save=9999.+fltarr(ndim)
        th0coin_save=9999.+fltarr(ndim)
        ho3coin_save=9999.+fltarr(ndim)
        so3coin_save=9999.+fltarr(ndim)
        agecoin_save=9999.+fltarr(ndim)
        dxcoin_save=9999.+fltarr(ndim)
        dycoin_save=9999.+fltarr(ndim)
        dtcoin_save=9999.+fltarr(ndim)
        idim=0L
        icount=0L
        first=0L
        for id=0,mday(im)-1 do begin
            lstdy=1
            leddy=mday(im)

            if id eq 0 and setplot eq 'ps' then begin
               lc=0
               set_plot,'ps'
               xsize=nxdim/100.
               ysize=nydim/100.
               !psym=0
               !p.font=0
               device,font_size=9
               device,/color,/landscape,bits=8,filename=$
                      'haloe_airmass_sage_sounding_'+start_date+'-'+end_date+'.ps'
               device,/inch,xoff=4.25-ysize/2.,yoff=5.5+xsize/2.,$
                      xsize=xsize,ysize=ysize
            endif

            FOR I=0,NORBIT-1 DO BEGIN
                sfile=diri+month(im)+string(FORMAT='(I2.2)',id)+'_'+$
                           string(FORMAT='(I4)',year(iy))+'_o3.sound.'+$
                           string(FORMAT='(I2.2)',i+1)

; if coincidence file exists open it
                close,4
                dum=findfile(sfile)
                if dum(0) ne '' then openr,4,sfile
                if dum(0) eq '' then goto,jumpover
                t=0.
                x=0.
                y=0.
                xs=0.
                ys=0.
                p_trop=0.
                z_trop=0.
                th_trop=0.
                m=0L
                readf,4,t,y,x,xs,ys,p_trop,z_trop,th_trop,m

                nl=0L
                readf,4,nl
                if first eq 0L then begin
                   start_date=strcompress(string(FORMAT='(I2.2)',lstmn),/remove_all)+$
                              strcompress(string(FORMAT='(I2.2)',lstdy),/remove_all)+$
                              strcompress(string(lstyr),/remove_all)
                   end_date=strcompress(string(FORMAT='(I2.2)',ledmn),/remove_all)+$
                            strcompress(string(FORMAT='(I2.2)',leddy),/remove_all)+$
                            strcompress(string(ledyr),/remove_all)
                   print,start_date,'-',end_date

                   tsage=9999.+fltarr(ndim)
                   xsage=9999.+fltarr(ndim)
                   ysage=9999.+fltarr(ndim)
                   tropp=9999.+fltarr(ndim)
                   tropz=9999.+fltarr(ndim)
                   tropth=9999.+fltarr(ndim)
                   mode=9999L+lonarr(ndim)
                   o3sage=9999.+fltarr(ndim,nl)
                   psage=9999.+fltarr(ndim,nl)
                   thsage=9999.+fltarr(ndim,nl)
                   zsage=9999.+fltarr(ndim,nl)
                   clsage=9999.+fltarr(ndim,nl)
                   qo3sage=9999.+fltarr(ndim,nl)
                   o3_sound=9999.+fltarr(nl)
                   p_sound=9999.+fltarr(nl)
                   th_sound=9999.+fltarr(nl)
                   z_sound=9999.+fltarr(nl)
                   cl_sound=9999.+fltarr(nl)
                   qo3_sound=9999.+fltarr(nl)
                   first=1
                endif
                tsage(idim)=t
                xsage(idim)=x
                ysage(idim)=y
                tropp(idim)=p_trop
                tropz(idim)=z_trop
                tropth(idim)=th_trop
                mode(idim)=m

                readf,4,o3_sound
                readf,4,p_sound
                readf,4,th_sound
                readf,4,z_sound
                readf,4,cl_sound
                readf,4,qo3_sound
                o3sage(idim,0:nl-1)=o3_sound
                psage(idim,0:nl-1)=p_sound
                thsage(idim,0:nl-1)=th_sound
                zsage(idim,0:nl-1)=z_sound
                clsage(idim,0:nl-1)=cl_sound
                qo3sage(idim,0:nl-1)=qo3_sound
                idim=idim+1

; read coincidences
                ncount=0L
                readf,4,ncount
                tcoin=9999.+fltarr(ncount)
                xcoin=9999.+fltarr(ncount)
                ycoin=9999.+fltarr(ncount)
                thcoin=9999.+fltarr(ncount)
                x0coin=9999.+fltarr(ncount)
                y0coin=9999.+fltarr(ncount)
                th0coin=9999.+fltarr(ncount)
                ho3coin=9999.+fltarr(ncount)
                so3coin=9999.+fltarr(ncount)
                agecoin=9999.+fltarr(ncount)
                dxcoin=9999.+fltarr(ncount)
                dycoin=9999.+fltarr(ncount)
                dtcoin=9999.+fltarr(ncount)
                readf,4,tcoin
                readf,4,xcoin
                readf,4,ycoin
                readf,4,thcoin
                readf,4,x0coin
                readf,4,y0coin
                readf,4,th0coin
                readf,4,so3coin
                readf,4,ho3coin
                readf,4,agecoin
                readf,4,dxcoin
                readf,4,dycoin
                readf,4,dtcoin
                close,4
 
                tcoin_save(icount:icount+ncount-1)=tcoin
                xcoin_save(icount:icount+ncount-1)=xcoin
                ycoin_save(icount:icount+ncount-1)=ycoin
                thcoin_save(icount:icount+ncount-1)=thcoin
                x0coin_save(icount:icount+ncount-1)=x0coin
                y0coin_save(icount:icount+ncount-1)=y0coin
                th0coin_save(icount:icount+ncount-1)=th0coin
                ho3coin_save(icount:icount+ncount-1)=ho3coin
                so3coin_save(icount:icount+ncount-1)=so3coin
                agecoin_save(icount:icount+ncount-1)=agecoin 
                dxcoin_save(icount:icount+ncount-1)=dxcoin
                dycoin_save(icount:icount+ncount-1)=dycoin
                dtcoin_save(icount:icount+ncount-1)=dtcoin
                icount=icount+ncount
                jumpover:
            ENDFOR		; loop over SAGE II soundings
        endfor			; loop over days
;
; truncate special values
;
        index=where(ho3coin_save ne 9999. and so3coin_save ne 9999.,ncount)
        if index(0) eq -1 then goto,jumpmonth
        tcoin_save=tcoin_save(index)
        xcoin_save=xcoin_save(index)
        ycoin_save=ycoin_save(index)
        thcoin_save=thcoin_save(index)
        x0coin_save=x0coin_save(index)
        y0coin_save=y0coin_save(index)
        th0coin_save=th0coin_save(index)
        ho3coin_save=ho3coin_save(index)
        so3coin_save=so3coin_save(index)
        agecoin_save=agecoin_save(index)
        dxcoin_save=dxcoin_save(index)
        dycoin_save=dycoin_save(index)
        dtcoin_save=dtcoin_save(index)
;
; find redundent coincidences and take only youngest one
;
        for mf=0,ncount-1 do begin
            if ho3coin_save(mf) ne 9999. then begin
               rindex=where(x0coin_save eq x0coin_save(mf) and $
                            y0coin_save eq y0coin_save(mf) and $ 
                            th0coin_save eq th0coin_save(mf) and $
                            ho3coin_save ne 9999.)
               if rindex(0) ne -1 then begin
                  agemin=min(agecoin_save(rindex))
                  rindex=where(x0coin_save eq x0coin_save(mf) and $
                               y0coin_save eq y0coin_save(mf) and $ 
                               th0coin_save eq th0coin_save(mf) and $
                               ho3coin_save ne 9999. and $
                               agecoin_save ne agemin)
               endif
            endif
        endfor
        gindex=where(so3coin_save ne 9999. and ho3coin_save ne 9999.,ncount)
        if gindex(0) ne -1 then begin
           print,'non-redundent number of coincidences= ',ncount
           xcoin_save=xcoin_save(gindex)
           ycoin_save=ycoin_save(gindex)
           thcoin_save=thcoin_save(gindex)
           x0coin_save=x0coin_save(gindex)
           y0coin_save=y0coin_save(gindex)
           th0coin_save=th0coin_save(gindex)
           tcoin_save=tcoin_save(gindex)
           ho3coin_save=ho3coin_save(gindex)
           so3coin_save=so3coin_save(gindex)
           agecoin_save=agecoin_save(gindex)
           dxcoin_save=dxcoin_save(gindex)
           dycoin_save=dycoin_save(gindex)
           dtcoin_save=dtcoin_save(gindex)
;
; scatter plot
;
           erase
           !type=2^2+2^3
           xmn=xorig(0)
           xmx=xorig(0)+xlen
           ymn=yorig(0)
           ymx=yorig(0)+ylen
           set_viewport,xmn,xmx,ymn,ymx
           !noeras=1
           !linetype=0
           mtitle='!6'+start_date+'-'+end_date+' Ozone 2x2 coincidences'
           plot,findgen(11),findgen(11),xrange=[0.,10.],yrange=[0.,10.],$
                xtitle='!6HALOE airmasses',ytitle='!6SAGE II soundings',$
                title=mtitle,charsize=1.5

           for ii=0,ncount-1 do $
               oplot,[ho3coin_save(ii)*1e6,ho3coin_save(ii)*1e6],$
                     [so3coin_save(ii)*1e6,so3coin_save(ii)*1e6],psym=8,$
                      color=((thcoin_save(ii)-320.)/(2100.-320.))*mcolor

           result=r_correlate(so3coin_save,ho3coin_save)
           r=result(0)
           s=result(1)
           xyouts,4.5,2.5,'!6Correlation ='+string(r),/data,charsize=1.5
           xyouts,4.5,1.7,'!6Significance='+string(s),/data,charsize=1.5
           xyouts,4.5,0.9,'!6N points    ='+string(ncount),/data,charsize=1.5

            imin=320.
            imax=2100.
            ymnb=yorig(0)-cbaryoff
            ymxb=ymnb+cbarydel
            set_viewport,xmn,xmx,ymnb,ymxb
            !type=2^2+2^3+2^6
            plot,[imin,imax],[0,0],yrange=[0,10],xrange=[imin,imax],$
                  xtitle='!6Potential Temperature (K)'
            ybox=[0,10,10,0,0]
            x1=imin
            nlvls=20
            col1=5+indgen(nlvls)*mcolor/(nlvls)
            dx=(imax-imin)/float(nlvls)
            for j=0,nlvls-1 do begin
                xbox=[x1,x1,x1+dx,x1+dx,x1]
                polyfill,xbox,ybox,color=col1(j)
                x1=x1+dx
            endfor
;
; histogram
;
           !type=2^2+2^3
           xmn=xorig(1)
           xmx=xorig(1)+xlen
           ymn=yorig(1)
           ymx=yorig(1)+ylen
           set_viewport,xmn,xmx,ymn,ymx
        
           !linetype=0
           x=.25*findgen(41)
           y1=histogram(ho3coin_save*1.e6,min=0,max=10,binsize=.25)/(1.*ncount)
           y1=smooth(y1,3)
           y2=histogram(so3coin_save*1.e6,min=0,max=10,binsize=.25)/(1.*ncount)
           y2=smooth(y2,3)
           ymax=max(y1,y2)+0.1*max(y1,y2)
           plot,x,y1,xtitle='!6      O3 (ppmv)',ytitle='!6Frequency',charsize=1.5,$
                title='!6HALOE Airmasses vs SAGE II Soundings PDFs',xrange=$
                [0.,10.],yrange=[0.,ymax]
           plots,[.2,.08],/normal
           plots,[.25,.08],/continue,/normal
           xyouts,.26,0.08,'HALOE Airmasses',/normal,charsize=1.5
           !linetype=1
           y2=histogram(so3coin_save*1.e6,min=0,max=10,binsize=.25)/(1.*ncount)
           y2=smooth(y2,3)
           oplot,x,y2
           plots,[.2,.05],/normal
           plots,[.25,.05],/continue,/normal
           !linetype=0
           xyouts,.26,0.05,'SAGE II Soundings',/normal,charsize=1.5
;
; calculate ks statistics
;
           ks_stats,ho3coin_save,so3coin_save,kstest,cprob
           xyouts,8,.95*ymax,'!6KS='+strmid(string(kstest),5,4),/data
           xyouts,8,.88*ymax,'!6KS sig='+$
                  strmid(string(100.*cprob),5,5)+'%',/data
           print,'KS=',kstest
           print,'KS significance=',100.*cprob,' %'
;
; calculate ks statistics with mean bias removed
;
           so3bar=total(so3coin_save)/n_elements(so3coin_save)
           ho3bar=total(ho3coin_save)/n_elements(ho3coin_save)
           ho3coin_save=ho3coin_save-ho3bar+so3bar
           ks_stats,ho3coin_save,so3coin_save,kstest,cprob
           xyouts,8,.8*ymax,'w/o Mean Bias:',/data
           xyouts,8,.73*ymax,'!6KS='+strmid(string(kstest),5,4),/data
           xyouts,8,.67*ymax,'!6KS sig='+strmid(string(100.*cprob),5,5)+'%',/data
           print,'KS=',kstest
           print,'KS significance=',100.*cprob,' %'

; Close PostScript file and return control to X-windows
           if setplot eq 'ps' then device, /close
        endif
        jumpmonth:
    endfor	; loop over months
endfor		; loop over years
end
