pro read_ace_airmass,iunit,ntraj,xn,yn,thn,agen,x0n,y0n,th0n,t0n,z0n,p0n,extn,$
       sadn,h2on,xno2n,o3n,extn_err,sadn_err,h2on_err,xno2n_err,o3n_err,pvn,$
       pn,msfn,zn,tmpn,qn,qdfn,markn,frday,frnght,xmint,minttime,vpn,sfn
char1='xtraj'
char2='ytraj'
char3='thtrj'
char4='agetr'
char5='x0trj'
char6='y0trj'
char7='th0tr'
char8='t0trj'
char9='z0trj'
char10='p0trj'
char11='exttrj'
char12='sadtrj'
char13='h2otrj'
char14='xno2trj'
char15='o3trj'
char16='exterr'
char17='saderr'
char18='h2oerr'
char19='xno2err'
char20='o3err'
char21='pvtrj'
char22='ptraj'
char23='msftr'
char24='ztraj'
char25='tmptj'
char26='qtraj'
char27='qdftr'
char28='mrktr'
char29='frday'
char30='frnght'
char31='xmint'
char32='tmint'
char33='vptraj'
char34='sftraj'
xn=fltarr(ntraj)
yn=fltarr(ntraj)
thn=fltarr(ntraj)
agen=fltarr(ntraj)
x0n=fltarr(ntraj)
y0n=fltarr(ntraj)
th0n=fltarr(ntraj)
t0n=fltarr(ntraj)
z0n=fltarr(ntraj)
p0n=fltarr(ntraj)
extn=fltarr(ntraj)
sadn=fltarr(ntraj)
h2on=fltarr(ntraj)
xno2n=fltarr(ntraj)
o3n=fltarr(ntraj)
extn_err=fltarr(ntraj)
sadn_err=fltarr(ntraj)
h2on_err=fltarr(ntraj)
xno2n_err=fltarr(ntraj)
o3n_err=fltarr(ntraj)
pvn=fltarr(ntraj)
pn=fltarr(ntraj)
msfn=fltarr(ntraj)
zn=fltarr(ntraj)
tmpn=fltarr(ntraj)
qn=fltarr(ntraj)
qdfn=fltarr(ntraj)
markn=fltarr(ntraj)
frday=fltarr(ntraj)
frnght=fltarr(ntraj)
xmint=fltarr(ntraj)
minttime=lonarr(ntraj)
vpn=fltarr(ntraj)
sfn=fltarr(ntraj)
readu,iunit,char1
readu,iunit,xn
readu,iunit,char2
readu,iunit,yn
readu,iunit,char3
readu,iunit,thn
readu,iunit,char4
readu,iunit,agen
readu,iunit,char5
readu,iunit,x0n
readu,iunit,char6
readu,iunit,y0n
readu,iunit,char7
readu,iunit,th0n
readu,iunit,char8
readu,iunit,t0n
readu,iunit,char9
readu,iunit,z0n
readu,iunit,char10
readu,iunit,p0n
readu,iunit,char11
readu,iunit,extn
readu,iunit,char12
readu,iunit,sadn
readu,iunit,char13
readu,iunit,h2on
readu,iunit,char14
readu,iunit,xno2n
readu,iunit,char15
readu,iunit,o3n
readu,iunit,char16
readu,iunit,extn_err
readu,iunit,char17
readu,iunit,sadn_err
readu,iunit,char18
readu,iunit,h2on_err
readu,iunit,char19
readu,iunit,xno2n_err
readu,iunit,char20
readu,iunit,o3n_err
readu,iunit,char21
readu,iunit,pvn
readu,iunit,char22
readu,iunit,pn
readu,iunit,char23
readu,iunit,msfn
readu,iunit,char24
readu,iunit,zn
readu,iunit,char25
readu,iunit,tmpn
readu,iunit,char26
readu,iunit,qn
readu,iunit,char27
readu,iunit,qdfn
readu,iunit,char28
readu,iunit,markn
readu,iunit,char29
readu,iunit,frday
readu,iunit,char30
readu,iunit,frnght
readu,iunit,char31
readu,iunit,xmint
readu,iunit,char32
readu,iunit,minttime
readu,iunit,char33
readu,iunit,vpn
readu,iunit,char34
readu,iunit,sfn
return
end