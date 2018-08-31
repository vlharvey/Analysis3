pro drawvectors1d,nvec,along,alat2,udata,vdata,maxmag,nh

    sina = udata/maxmag
    cosa = vdata/maxmag
      for i=0,nvec-1 do begin
        x0=along(i)
        y0=alat2(i)
        dx = sina(i)
        dy = cosa(i)
        if dx ge 999. then goto, clipdata
        size = sqrt(dx*dx+dy*dy)
        arr = .25
        st = arr*.38268
        ct = arr*.92387
        x1 = x0 + dx
        y1 = y0 + dy
        if y1 gt 90. then begin
           y1=90.-(y1-90.)
           x1=x1+180.
        endif
        if y1 lt -90. then begin
           y1=-90.-(y1+90.)
           x1=x1+180.
        endif
        if x1 gt 360. then x1=x1-360.
        if y0 ge 90. or y0 lt -90. then goto, jumplab
        oplot,[x0,x1],[y0,y1],color=255,thick=2
        oplot,[x1,x1-(ct*dx-st*dy)],[y1,y1-(ct*dy+st*dx)],color=255,thick=2
        oplot,[x1,x1-(ct*dx+st*dy)],[y1,y1-(ct*dy-st*dx)],color=255,thick=2
jumplab:
clipdata:
    endfor

end
