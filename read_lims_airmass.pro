pro read_lims_airmass,iunit,ntraj,xn,yn,thn,agen,x0n,y0n,th0n,t0n,z0n,p0n,$
       h2on,xno2n,o3n,hno3n,pvn,pn,msfn,zn,tmpn,qn,qdfn,markn,frday,frnght,$
       xmint,minttime,vpn,sfn
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
char11='h2otrj'
char12='xno2trj'
char13='o3trj'
char14='hno3trj'
char15='pvtrj'
char16='ptraj'
char17='msftr'
char18='ztraj'
char19='tmptj'
char20='qtraj'
char21='qdftr'
char22='mrktr'
char23='frday'
char24='frnght'
char25='xmint'
char26='tmint'
char27='vptraj'
char28='sftraj'
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
h2on=fltarr(ntraj)
xno2n=fltarr(ntraj)
o3n=fltarr(ntraj)
hno3n=fltarr(ntraj)
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
readu,iunit,h2on
readu,iunit,char12
readu,iunit,xno2n
readu,iunit,char13
readu,iunit,o3n
readu,iunit,char14
readu,iunit,hno3n
readu,iunit,char15
readu,iunit,pvn
readu,iunit,char16
readu,iunit,pn
readu,iunit,char17
readu,iunit,msfn
readu,iunit,char18
readu,iunit,zn
readu,iunit,char19
readu,iunit,tmpn
readu,iunit,char20
readu,iunit,qn
readu,iunit,char21
readu,iunit,qdfn
readu,iunit,char22
readu,iunit,markn
readu,iunit,char23
readu,iunit,frday
readu,iunit,char24
readu,iunit,frnght
readu,iunit,char25
readu,iunit,xmint
readu,iunit,char26
readu,iunit,minttime
readu,iunit,char27
readu,iunit,vpn
readu,iunit,char28
readu,iunit,sfn
return
end