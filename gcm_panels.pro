pro gcm_panels,npp,delta,nxdim,nydim,xorig,yorig,xlen,ylen,cbaryoff,cbarydel

;------------------------------------------------------------------------
;
; procedure to set positions for multi-panel plots in gcm plot codes
;
; input parameters:
;
;   npp           integer            number of panels per page
;   delta         string             flag, =y to generate delta plot
;
; returned parameters:
;
;   nxdim         integer            window x-dimension in pixels
;   nydim         integer            window y-dimension in pixels
;   xorig         fltarr(npp)        x coordinates of plot origins
;   yorig         fltarr(npp)        y coordinates of plot origins
;   xlen          real               length of x axis
;   ylen          real               length of y axis
;   cbaryoff      real               y distance between plot origin
;                                     and colorbar origin
;   cbarydel      real               thickness of colorbar
;
;------------------------------------------------------------------------

;C  define viewport location for each panel
case 1 of
  (npp eq 1): begin
    nxdim=750
    nydim=750
    xorig=[0.15]
    yorig=[0.2]
    xlen=0.7
    ylen=0.7
    cbaryoff=0.1
    cbarydel=0.01
  end
  (npp eq 2): begin
    nxdim=750
    nydim=750
    xorig=[.2,.2]
    yorig=[.6,.2]
    xlen=0.6
    ylen=0.3
    cbaryoff=0.02
    cbarydel=0.01
  end
  (npp eq 3): begin
    nxdim=750
    nydim=1000
    yorig=[.7,.4,.1]
    xorig=[.2,.2,.2]
    xlen=0.7
    ylen=0.25
    cbaryoff=0.05
    cbarydel=0.01
  end
  (npp eq 4): begin
    nxdim=800
    nydim=800
    xorig=[0.1,0.55,0.1,0.55]
    yorig=[0.6,0.6,0.15,0.15]
    xlen=0.35
    ylen=0.3
    cbaryoff=0.06
    cbarydel=0.01
  end
  (npp eq 5 or npp eq 6): begin
    nxdim=750
    nydim=600
    xorig=[.01,.34,.67,.01,.34,.67]
    yorig=[.55,.55,.55,.10,.10,.10]
    xlen=0.3
    ylen=0.4
    cbaryoff=0.03
    cbarydel=0.01
; (npp eq 6): begin
;   nxdim=750
;   nydim=600
;   xorig=[.025,.375,.7,.025,.375,.7]
;   yorig=[.55,.55,.55,.1,.1,.1]
;   xlen=0.3
;   ylen=0.3
;   cbaryoff=0.055
;   cbarydel=0.005
  end
endcase

end
