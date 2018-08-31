pro rd_socrates_o3_soundings_julian,nday,norbit,$
    hfile,dir,thal,xhal,yhal,thhal,o3hal,modehal

month=['jan','feb','mar','apr','may','jun',$
       'jul','aug','sep','oct','nov','dec']
mday=[31,28,31,30,31,30,31,31,30,31,30,31]
icount=0L
for n=0,nday-1 do begin
    close,4
    openr,4,dir+hfile(n)
    print,'reading socrates ',hfile(n)

; determine if it is a leap year for Julian hour calculation
    leapyr=(fix(strmid(hfile(n),7,2)) mod 4)
    if leapyr eq 0 then leapdy=1
    if leapyr ne 0 then leapdy=0
    imn=where(month eq strmid(hfile(n),0,3))
    if imn(0) lt 2 then leapdy=0
    mdays=0
    for i=0,imn(0)-1 do begin
        mdays=mdays+mday(i)
    endfor
    jday=mdays+fix(strmid(hfile(n),4,2))+leapdy

    readf,4,norbit
;   print,'number of orbits is ',norbit
    for i=0,norbit-1 do begin
        x=0.
        y=0.
        d=0.
        t=0.
        p_trop=0.
        z_trop=0.
        th_trop=0.
        m=0L

; time in Julian hours
        readf,4,t,y,x,d,p_trop,z_trop,th_trop,m
;       print,t,y,x,d,p_trop,z_trop,th_trop,m
        thal(icount,*)=24.*jday+t
        xhal(icount,*)=x
        yhal(icount,*)=y
        modehal(icount,*)=m

        readf,4,nl
        o3_snd=fltarr(nl) 
        p_snd=fltarr(nl) 
        th_snd=fltarr(nl) 
        z_snd=fltarr(nl) 
        cl_snd=fltarr(nl)
        qo3_snd=fltarr(nl) 

        readf,4,o3_snd
        readf,4,p_snd
        readf,4,th_snd
        readf,4,z_snd
        readf,4,cl_snd
        readf,4,qo3_snd
        o3hal(icount,0:nl-1)=o3_snd
        thhal(icount,0:nl-1)=th_snd
        icount=icount+1L
    endfor
    close,4
endfor
return
end
