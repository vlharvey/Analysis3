      program vis5d

      implicit none

c --- This program, v5dconvert_UKMO_theta.f, converts isentropic 
c --- gridded UKMO analyses of pv,p,u,v,qdf,q,msf,mark,z,tmp into
c --- lat, lon, and HEIGHT arrays for VIS5D.

c include file for netCDF functions
      include "/usr/local/include/netcdf.inc"

      integer nc,nr,nth,klv
      parameter(nc=96,nr=72,nth=24,klv=30)

c 3d UKMO isentropic data
      real ipvgrd(nr,nc,nth),pgrd(nr,nc,nth),
     +       ugrd(nr,nc,nth),vgrd(nr,nc,nth),
     +     qdfgrd(nr,nc,nth),qgrd(nr,nc,nth),
     +     xmsfgrd(nr,nc,nth),xmarkgrd(nr,nc,nth),
     +     vpgrd(nr,nc,nth),sfgrd(nr,nc,nth),
     +     zgrd(nr,nc,nth),tmpgrd(nr,nc,nth),sgrd(nr,nc,nth)

      common /ukmoth/ ipvgrd,pgrd,ugrd,vgrd,qdfgrd,qgrd,
     +       xmsfgrd,xmarkgrd,vpgrd,sfgrd,zgrd,tmpgrd,sgrd

c 3d UKMO constant height arrays
      real vipvz(nr,nc+1,klv),vpz(nr,nc+1,klv),vuz(nr,nc+1,klv)
      real vvz(nr,nc+1,klv),vsz(nr,nc+1,klv),vqz(nr,nc+1,klv)
      real vqdfz(nr,nc+1,klv),vmsfz(nr,nc+1,klv)
      real vvpz(nr,nc+1,klv),vsfz(nr,nc+1,klv)
      real vmarkz(nr,nc+1,klv),vtz(nr,nc+1,klv),vzz(nr,nc+1,klv)

      common /visdata/ vipvz,vpz,vuz,vvz,vsz,vqz,vqdfz,
     +                 vmsfz,vmarkz,vvpz,vsfz,vtz,vzz

      real alon(nc),alat(nr),thlev(nth)
      common /grddata/ alon,alat,thlev

      integer lstmn,lstdy,lstyr,ledmn,leddy,ledyr,kgmt,ndays
      integer lstday,ledday,stddat,iyr1,idy,imn,iyr,iday,iret
      integer ncid,nrw,ncw,nthw

      real arr1(nc),arr2(nr),arr3(nth),arr4(nr,nc,nth)
      real arr5(nr,nc,nth),arr6(nr,nc,nth),arr7(nr,nc,nth)
      real arr8(nr,nc,nth),arr9(nr,nc,nth),arr10(nr,nc,nth)
      real arr11(nr,nc,nth),arr12(nr,nc,nth),arr13(nr,nc,nth)

      logical ex

c     character hdr*23,ifile*36,name*20,mon(12)*4
      character hdr*36,ifile*49,name*20,mon(12)*4

      data mon /'jan_','feb_','mar_','apr_','may_','jun_',
     +          'jul_','aug_','sep_','oct_','nov_','dec_'/
c theta levels
      data thlev /2000.,1800.,1600.,1400.,1200.,1000., 900.,
     + 800., 700., 600., 550., 525., 500., 475., 450., 425.,
     + 400., 390., 380., 370., 360., 350., 340., 330./
      data hdr /'/aura3/data/UKMO_data/Datfiles/ukmo_'/
      data ex /.false./

20    write (6,'(A,$)') ' Enter starting date (mm dd yyyy) '
      read  (*,*) lstmn,lstdy,lstyr

40    write (6,'(A,$)') ' Enter ending date (mm dd yyyy) '
      read  (*,*) ledmn,leddy,ledyr

      if (lstyr.lt.1991 .or. lstyr.gt.2007) stop 'Year out of range '
      if (ledyr.lt.1991 .or. ledyr.gt.2007) stop 'Year out of range '
      lstday = stddat(lstmn,lstdy,lstyr)
      ledday = stddat(ledmn,leddy,ledyr)

c -   Compute initial Julian date
      iyr = lstyr
      idy = lstdy
      imn = lstmn
      iday = kgmt(imn,idy,iyr)
      iday = iday - 1

c --- Loop here --------
500   iday = iday + 1
      call kdate (float(iday),iyr,imn,idy)
      call ckday (iday,iyr)
      if (iyr.lt.2000) iyr1 = iyr - 1900
      if (iyr.ge.2000) iyr1 = iyr - 2000
      print *,imn,idy,iyr

c --- Test for end condition and close windows.
      ndays = stddat(imn,idy,iyr)
      if (ndays.lt.lstday) stop ' starting day outside range '
      if (ndays.gt.ledday) stop ' Normal termination condition '

      write(ifile,'(a36,a4,i2.2,a1,i2.2,a4)')
     +      hdr,mon(imn),idy,'_',iyr1,'.nc3'
      inquire(file=ifile,exist=ex)

c open netCDF dataset w/o write privileges
      if (ex) then
          ncid = ncopn(ifile, NCNOWRIT, iret)
          print *,'opening ',ifile
         call ncdinq(ncid, 1, name, nrw,  iret)
         call ncdinq(ncid, 2, name, ncw,  iret)
         call ncdinq(ncid, 3, name, nthw, iret)
         write(6,*)'netCDF dimensions ',nrw,ncw,nthw
         call rdwind(ncid,arr1,arr2,arr3,arr4,arr5,
     1        arr6,arr7,arr8,arr9,arr10,arr11,arr12,
     2        arr13,nrw,ncw,nthw)
      endif

c --- convert raw and derived variables to the proper domain
      call makevis5darray
      call makv5dwkspiel(iyr,iday)

      goto 500
      end
c ****************************************
      subroutine rdwind(ncid,arr1,arr2,arr3,arr4,arr5,
     1           arr6,arr7,arr8,arr9,arr10,arr11,arr12,
     2           arr13,nrw,ncw,nthw)
c
c*****************************************************************c
c                                                                 c
c   Read flow field data                                          c
c                                                                 c
c*****************************************************************c
c
      include "/usr/local/include/netcdf.inc"

      integer nc,nr,nth,klv
      parameter(nc=96,nr=72,nth=24,klv=30)

c 3d UKMO isentropic data
      real ipvgrd(nr,nc,nth),pgrd(nr,nc,nth),
     +       ugrd(nr,nc,nth),vgrd(nr,nc,nth),
     +     qdfgrd(nr,nc,nth),qgrd(nr,nc,nth),
     +     xmsfgrd(nr,nc,nth),xmarkgrd(nr,nc,nth),
     +     vpgrd(nr,nc,nth),sfgrd(nr,nc,nth),
     +     zgrd(nr,nc,nth),tmpgrd(nr,nc,nth),sgrd(nr,nc,nth)

      common /ukmoth/ ipvgrd,pgrd,ugrd,vgrd,qdfgrd,qgrd,
     +        xmsfgrd,xmarkgrd,vpgrd,sfgrd,zgrd,tmpgrd,sgrd

      real alon(nc),alat(nr),thlev(nth)
      common /grddata/ alon,alat,thlev

      integer istart(3),icount(3)
      dimension arr1(ncw),arr2(nrw),arr3(nthw)
      dimension arr4(nrw,ncw,nthw),arr5(nrw,ncw,nthw),
     1          arr6(nrw,ncw,nthw),arr7(nrw,ncw,nthw),
     2          arr8(nrw,ncw,nthw),arr9(nrw,ncw,nthw),
     3          arr10(nrw,ncw,nthw),arr11(nrw,ncw,nthw),
     4          arr12(nrw,ncw,nthw),arr13(nrw,ncw,nthw)
      data istart/1,1,1/
      icount(1)=nrw
      icount(2)=ncw
      icount(3)=nthw

      write(6,*)' READING 3D UKMO ISENTROPIC DATA ......'

c --- 1=lons, 2=lats, 3=thetas
      call ncvgt(ncid, 1, 1, ncw, arr1, iret)
      print *,arr1
      call ncvgt(ncid, 2, 1, nrw, arr2, iret)
      print *,arr2
      call ncvgt(ncid, 3, 1, nthw, arr3, iret)
      print *,arr3
c --- 3d data
      call ncvgt(ncid, 4, istart, icount, arr4, iret)
      print *,'PV ',arr4(1,1,1)
      call ncvgt(ncid, 5, istart, icount, arr5, iret)
      print *,'P  ',arr5(1,1,1)
      call ncvgt(ncid, 6, istart, icount, arr6, iret)
      print *,'MSF ',arr6(1,1,1)
      call ncvgt(ncid, 7, istart, icount, arr7, iret)
      print *,'U  ',arr7(1,1,1)
      call ncvgt(ncid, 8, istart, icount, arr8, iret)
      print *,'V  ',arr8(1,1,1)
      call ncvgt(ncid, 9, istart, icount, arr9, iret)
      print *,'DTHDT ',arr9(1,1,1)
      call ncvgt(ncid, 10, istart, icount, arr10, iret)
      print *,'QDF ',arr10(1,1,1)
      call ncvgt(ncid, 11, istart, icount, arr11, iret)
      print *,'MARK ',arr11(1,1,1)
      call ncvgt(ncid, 12, istart, icount, arr12, iret)
      print *,'VP ',arr12(1,1,1)
      call ncvgt(ncid, 13, istart, icount, arr13, iret)
      print *,'SF ',arr13(1,1,1)

c close netCDF dataset
      call ncclos(ncid, iret)
C
      dlon=arr1(2)-arr1(1)
      dlat=arr2(2)-arr2(1)
      do i=1,nc
         alon(i)=arr1(i)
      enddo
      do j=1,nr
         alat(j)=arr2(j)
      enddo
      do k=1,nth
         thlev(k)=arr3(k)
      enddo
      do i=1,nc
        do j=1,nr
          do l=1,nth
            ipvgrd(j,i,l)=arr4(j,i,l)
              pgrd(j,i,l)=arr5(j,i,l)
           xmsfgrd(j,i,l)=arr6(j,i,l)
              ugrd(j,i,l)=arr7(j,i,l)
              vgrd(j,i,l)=arr8(j,i,l)
C Heating rate is in K/day 
              qgrd(j,i,l)=arr9(j,i,l)
c QDF deformation diagnostic				Duncan  7/14/99
            qdfgrd(j,i,l)=arr10(j,i,l)
c Streamfunction Vortex Marker (-1=AC, 0=none, +1=CY)	VLH	12/5/00
          xmarkgrd(j,i,l)=arr11(j,i,l)
c Velocity Potential					VLH     12/5/00
             vpgrd(j,i,l)=arr12(j,i,l)
c Streamfunction					VLH     12/5/00
             sfgrd(j,i,l)=arr13(j,i,l)
c Calculate temperature and height of theta surface
            tmpgrd(j,i,l) = thlev(l)*( (pgrd(j,i,l)/1000.)**(.286) )
              zgrd(j,i,l) = (xmsfgrd(j,i,l) - 1004.*tmpgrd(j,i,l))/
     +                                (9.86*1000.)
              sgrd(j,i,l) = sqrt(ugrd(j,i,l)**2.+vgrd(j,i,l)**2.)

c     print *,'pv,p,m,u,v,q ',i,j,l,ipvgrd(i,j,l),pgrd(i,j,l),
c    +  xmsfgrd(i,j,l),ugrd(i,j,l),vgrd(i,j,l),sgrd(i,j,l),
c    +  qgrd(i,j,l),qdfgrd(i,j,l),xmarkgrd(i,j,l),
c    +  vpgrd(i,j,l),sfgrd(i,j,l),tmpgrd(i,j,l),zgrd(i,j,l)
          enddo
        enddo
c     stop
      enddo
c     print *,'pv,p,m,u,v,q ',ipvgrd(1,1,1),pgrd(1,1,1),
c    +  xmsfgrd(1,1,1),ugrd(1,1,1),vgrd(1,1,1),sgrd(1,1,1),
c    +  qgrd(1,1,1),qdfgrd(1,1,1),xmarkgrd(1,1,1),
c    +  tmpgrd(1,1,1),zgrd(1,1,1)

      return
      end
c ****************************************
      subroutine kdate (gmt,iyr,imn,iday)
      real gmt
      dimension month(12)
      data month /31,28,31,30,31,30,31,31,30,31,30,31/

      i = 0
      igmt = int(gmt)
      if (mod(iyr,4).eq.0) then
         month(2) = 29
         if (igmt.eq.367) then
          imn = 1
          iday = 1
          goto 20
         endif
       else
         month(2) = 28
         if (igmt.eq.366) then
          imn = 1
          iday = 1
          goto 20
         endif
       endif
c
      do j = 1,12
         i = i + month(j)
         if (i.gt.igmt) then
             imn = j
             iday = igmt + month(j) - i
             goto 20
         endif
         if (i.eq.igmt) then
             imn = j
             iday = month(j)
             goto 20
         endif
      enddo

  20  return
      end
c ****************************************
      integer function stddat(imn,iday,iyr)

c --- Determines the number of days since Jan 1, 1956.
      dimension month(12)
      data month /31,28,31,30,31,30,31,31,30,31,30,31/
      data ibase /1956/

c --- Compute number of days since 1 January 1956
      idays = float(iyr-ibase)*365.

c --- test for leap year
c --- add the extra days for the period from 1956
      if (mod(iyr,4).eq.0) then
       month(2) = 29
       leapdays = int (float(iyr-1956)/4. )
      else
       month(2) = 28
       leapdays = int (float(iyr-ibase)/4.) + 1 
      endif

      if (imn.eq.1) goto 20

      do 10 j = 1,imn-1
       idays = idays + month(j)
  10  continue

  20  stddat = iday + idays + leapdays

      return
      end
c ****************************************
      subroutine makevis5darray

      implicit none

      integer nc,nr,nth,klv
      parameter(nc=96,nr=72,nth=24,klv=30)

c 3d UKMO isentropic data
      real ipvgrd(nr,nc,nth),pgrd(nr,nc,nth),ugrd(nr,nc,nth),
     +     vgrd(nr,nc,nth),qdfgrd(nr,nc,nth),qgrd(nr,nc,nth),
     +     xmsfgrd(nr,nc,nth),xmarkgrd(nr,nc,nth),
     +     vpgrd(nr,nc,nth),sfgrd(nr,nc,nth),zgrd(nr,nc,nth),
     +     tmpgrd(nr,nc,nth),sgrd(nr,nc,nth)

      common /ukmoth/ ipvgrd,pgrd,ugrd,vgrd,qdfgrd,qgrd,
     +        xmsfgrd,xmarkgrd,vpgrd,sfgrd,zgrd,tmpgrd,sgrd

      real alon(nc),alat(nr),thlev(nth)
      common /grddata/ alon,alat,thlev

c 3d UKMO constant height arrays
      real vipvz(nr,nc+1,klv),vpz(nr,nc+1,klv),vuz(nr,nc+1,klv)
      real vvz(nr,nc+1,klv),vsz(nr,nc+1,klv),vqz(nr,nc+1,klv)
      real vqdfz(nr,nc+1,klv),vmsfz(nr,nc+1,klv)
      real vvpz(nr,nc+1,klv),vsfz(nr,nc+1,klv)
      real vmarkz(nr,nc+1,klv),vtz(nr,nc+1,klv),vzz(nr,nc+1,klv)

      common /visdata/ vipvz,vpz,vuz,vvz,vsz,vqz,vqdfz,
     +                 vmsfz,vmarkz,vvpz,vsfz,vtz,vzz

      real xaipv(nth),xap(nth),xau(nth),xav(nth)
      real xaq(nth),xaqdf(nth),xamsf(nth),xamark(nth),xat(nth)
      real xavp(nth),xasf(nth)

      real xaz(nth),xbz(klv)

      real xbipv(klv),xbp(klv),xbu(klv),xbv(klv)
      real xbq(klv),xbqdf(klv),xbmsf(klv),xbmark(klv),xbt(klv)
      real xbvp(nth),xbsf(nth)

      real tempipv,tempp,tempu,tempv,tempq,tempqdf,scale
      real tempmsf,tempmark,tempvp,tempsf,tempt,tempz

      integer i,j,k,l,jj

c Vis5d heights range from 10 km to 40 km
      do k = 1,klv
         xbz(k) = k+10.0
      enddo

c --- linearly interpolate data vertically to evenly spaced height coordinates.
      do i = 1, nc
         do j = 1, nr

c for each vertical profile
            do k = 1, nth
               xaz(k)   =zgrd(j,i,k)
c              print *,'i,j,k ',i,j,k,'  Z= ',xaz(k)
               xaipv(k) =ipvgrd(j,i,k)
c              print *,'k ',k,xaz(k),xaipv(k)
               xap(k)   =pgrd(j,i,k)
               xau(k)   =ugrd(j,i,k)
               xav(k)   =vgrd(j,i,k)
               xaq(k)   =qgrd(j,i,k)
               xaqdf(k) =qdfgrd(j,i,k)
               xamsf(k) =xmsfgrd(j,i,k)
               xamark(k)=xmarkgrd(j,i,k)
               xavp(k)  =vpgrd(j,i,k)
               xasf(k)  =sfgrd(j,i,k)
               xat(k)   =tmpgrd(j,i,k)
            enddo

            do k = 1,klv
c              print *,'k ',k
c stop 8 levels before bottom.  height of theta is not monotonic in low trop
               do l=1,nth-1
c                 print *,'l ',l,xaz(l),xbz(k),xaz(l+1)
c                 if( (xbz(k).ge.xaz(l).and.xbz(k).lt.xaz(l+1)) )then

c level 1 is the top
                  if( (xbz(k).le.xaz(l).and.xbz(k).gt.xaz(l+1)) )then
                     scale=(xbz(k)-xaz(l))/(xaz(l+1)-xaz(l))
c                    print *,'interpolating',k,l,xaz(l),xbz(k),xaz(l+1)
                     xbipv(k) = xaipv(l)+scale*(xaipv(l+1)-xaipv(l))
c                    print *,'interp',k,l,xaipv(l),xbipv(k),xaipv(l+1)
                     xbp(k)   = xap(l)+scale*(xap(l+1)-xap(l))
                     xbu(k)   = xau(l)+scale*(xau(l+1)-xau(l))
                     xbv(k)   = xav(l)+scale*(xav(l+1)-xav(l))
                     xbq(k)   = xaq(l)+scale*(xaq(l+1)-xaq(l))
                     xbqdf(k) = xaqdf(l)+scale*(xaqdf(l+1)-xaqdf(l))
                     xbmsf(k) = xamsf(l)+scale*(xamsf(l+1)-xamsf(l))
                     xbmark(k)= xamark(l)+scale*(xamark(l+1)-xamark(l))
                     xbvp(k)  = xavp(l)+scale*(xavp(l+1)-xavp(l))
                     xbsf(k)  = xasf(l)+scale*(xasf(l+1)-xasf(l))
                     xbt(k)   = xat(l)+scale*(xat(l+1)-xat(l))
                     goto 20
                  endif
               enddo
20             continue
               vipvz(j,i,k) = xbipv(k)
               vpz(j,i,k)   = xbp(k)
               vuz(j,i,k)   = xbu(k)
               vvz(j,i,k)   = xbv(k)
               vqz(j,i,k)   = xbq(k)
               vqdfz(j,i,k) = xbqdf(k)
               vmsfz(j,i,k) = xbmsf(k)
               vmarkz(j,i,k)= xbmark(k)
               vvpz(j,i,k)  = xbvp(k)
               vsfz(j,i,k)  = xbsf(k)
               vtz(j,i,k)   = xbt(k)
               vzz(j,i,k)   = xbz(k)
            enddo
         enddo
      enddo

c --- Rearrange latitudes and longitudes for vis5D
c --- i = 1 --> 180 and j = 1 --> 0N
      do k = 1, klv
         do j = 1, nr
            jj=nr-j+1
            do i = 1, nc/2
               tempipv  = vipvz(jj,i,k)
               tempp    = vpz(jj,i,k)
               tempu    = vuz(jj,i,k)
               tempv    = vvz(jj,i,k)
               tempq    = vqz(jj,i,k)
               tempqdf  = vqdfz(jj,i,k)
               tempmsf  = vmsfz(jj,i,k)
               tempmark = vmarkz(jj,i,k)
               tempvp   = vvpz(jj,i,k)
               tempsf   = vsfz(jj,i,k)
               tempt    = vtz(jj,i,k)
               tempz    = vzz(jj,i,k)

               vipvz(jj,i,k) = vipvz(j,i+nc/2,k)
               vpz(jj,i,k)   = vpz(j,i+nc/2,k)
               vuz(jj,i,k)   = vuz(j,i+nc/2,k)
               vvz(jj,i,k)   = vvz(j,i+nc/2,k)
               vqz(jj,i,k)   = vqz(j,i+nc/2,k)
               vqdfz(jj,i,k) = vqdfz(j,i+nc/2,k)
               vmsfz(jj,i,k) = vmsfz(j,i+nc/2,k)
               vmarkz(jj,i,k)= vmarkz(j,i+nc/2,k)
               vvpz(jj,i,k)  = vvpz(j,i+nc/2,k)
               vsfz(jj,i,k)  = vsfz(j,i+nc/2,k)
               vtz(jj,i,k)   = vtz(j,i+nc/2,k)
               vzz(jj,i,k)   = vzz(j,i+nc/2,k)
               vsz(jj,i,k)   = sqrt(vuz(j,i+nc/2,k)**2+
     +                              vvz(j,i+nc/2,k)**2)

               vipvz(j,i+nc/2,k) = tempipv
               vpz(j,i+nc/2,k)   = tempp
               vuz(j,i+nc/2,k)   = tempu
               vvz(j,i+nc/2,k)   = tempv
               vqz(j,i+nc/2,k)   = tempq
               vqdfz(j,i+nc/2,k) = tempqdf
               vmsfz(j,i+nc/2,k) = tempmsf
               vmarkz(j,i+nc/2,k)= tempmark
               vvpz(j,i+nc/2,k)  = tempvp
               vsfz(j,i+nc/2,k)  = tempsf
               vtz(j,i+nc/2,k)   = tempt
               vzz(j,i+nc/2,k)   = tempz
               vsz(j,i+nc/2,k)   = sqrt(tempu**2+tempv**2)
c              print *,j,i,k,vipvz(j,i,k),vmarkz(j,i,k)
            enddo
            vipvz(j,nc+1,k) = vipvz(j,1,k)
            vpz(j,nc+1,k)   = vpz(j,1,k)
            vuz(j,nc+1,k)   = vuz(j,1,k)
            vvz(j,nc+1,k)   = vvz(j,1,k)
            vqz(j,nc+1,k)   = vqz(j,1,k)
            vqdfz(j,nc+1,k) = vqdfz(j,1,k)
            vmsfz(j,nc+1,k) = vmsfz(j,1,k)
            vmarkz(j,nc+1,k)= vmarkz(j,1,k)
            vvpz(j,nc+1,k)  = vvpz(j,1,k)
            vsfz(j,nc+1,k)  = vsfz(j,1,k)
            vtz(j,nc+1,k)   = vtz(j,1,k)
            vzz(j,nc+1,k)   = vzz(j,1,k)
            vsz(j,nc+1,k)   = vsz(j,1,k)
         enddo
c        print *,k,vipvz(2,2,k),vpz(2,2,k),vuz(2,2,k),
c    +           vvz(2,2,k),vqz(2,2,k),vqdfz(2,2,k),
c    +           vmsfz(2,2,k),vmarkz(2,2,k),
c    +           vvpz(2,2,k),vsfz(2,2,k),
c    +           vtz(2,2,k),vsz(2,2,k),vzz(2,2,k)
      enddo

      return
      end
c ****************************************
      subroutine makv5dwkspiel(iy,id)
c
c --- Note that rows are now latitudes and columns are longitudes !

      implicit none

      integer nc,nr,nth,klv
      parameter(nc=96,nr=72,nth=24,klv=30)

      include "/usr/dist/build_sw/vis5d+-1.2.1/src/v5df.h"

c 3d UKMO constant height arrays
      real vipvz(nr,nc+1,klv),vpz(nr,nc+1,klv),vuz(nr,nc+1,klv)
      real vvz(nr,nc+1,klv),vsz(nr,nc+1,klv),vqz(nr,nc+1,klv)
      real vqdfz(nr,nc+1,klv),vmsfz(nr,nc+1,klv)
      real vvpz(nr,nc+1,klv),vsfz(nr,nc+1,klv)
      real vmarkz(nr,nc+1,klv),vtz(nr,nc+1,klv),vzz(nr,nc+1,klv)

      common /visdata/ vipvz,vpz,vuz,vvz,vsz,vqz,vqdfz,
     +                 vmsfz,vmarkz,vvpz,vsfz,vtz,vzz

      real proj_args(4),vert_args(2)
      real northlat,latinc,westlon,loninc,bottomhgt,hgtinc

      character*100 outname
      character*10 varname(30)

      integer n,iy,id
      integer nl,i,numvars,date,time
      integer nlvar(13),compressmode,projection,vertical

      data nl / klv /
c     data numvars / 13 /
      data numvars / 7 /
c     data (varname(i),i=1,13) / 'IPV','PRS',' U ',' V ',' S ',
c    +     ' Q ','QDF','MSF','MK1','MK2',' VP',' SF',' T '/
      data (varname(i),i=1,7) / ' U ',' V ',' S ',' Q ',
     +      'MK1','MK2',' T '/
      data nlvar / klv,klv,klv,klv,klv,klv,klv,
     +             klv,klv,klv,klv,klv,klv /
      data northlat, latinc / 88.75, 2.5 /
      data westlon, loninc  / 181.875, 3.75 /
c     data bottomhgt, hgtinc / 10.,1. /
      data bottomhgt, hgtinc / 100.,10000. /

      date=iy*1000+id
      time=180000
      compressmode = 1
      projection = 1
      vertical = 1
      proj_args(1) = northlat
      proj_args(2) = westlon
      proj_args(3) = latinc
      proj_args(4) = loninc
      vert_args(1) = bottomhgt
      vert_args(2) = hgtinc

c Create the v5d file.
      write(outname,120)iy,id,time
120   format('v5d.ukmo.',i4,'.',i3.3,'.',i6)
      print*,'writing vis5d file: ',outname
      n = v5dCreate( outname, 1, numvars, nr, nc+1, nlvar,
     &               varname, time, date, compressmode,
     &               projection, proj_args, vertical, vert_args )
      if (n .eq. 0) then
         call exit(1)
      endif

C Write out potential vorticity
c     n = v5dwrite( 1, 1, vipvz )
c     if (n .eq. 0) call exit(1)
C Write out pressure
c     n = v5dwrite( 1, 2, vpz )
c     if (n .eq. 0) call exit(1)
C Write out zonal wind
      n = v5dwrite( 1, 1, vuz )
      if (n .eq. 0) call exit(1)
C Write out meridonal wind
      n = v5dwrite( 1, 2, vvz )
      if (n .eq. 0) call exit(1)
C Write out wind speed
      n = v5dwrite( 1, 3, vsz )
      if (n .eq. 0) call exit(1)
C Write out theta dot
      n = v5dwrite( 1, 4, vqz )
      if (n .eq. 0) call exit(1)
C Write out QDF
c     n = v5dwrite( 1, 7, vqdfz )
c     if (n .eq. 0) call exit(1)
C Write out MSF
c     n = v5dwrite( 1, 8, vmsfz )
c     if (n .eq. 0) call exit(1)
C Write out Mark
      n = v5dwrite( 1, 5, vmarkz )
      if (n .eq. 0) call exit(1)
c again
      n = v5dwrite( 1, 6, vmarkz )
      if (n .eq. 0) call exit(1)
c Write out velocity potential
c     n = v5dwrite( 1, 11, vvpz )
c     if (n .eq. 0) call exit(1)
c Write out streamfunction
c     n = v5dwrite( 1, 12, vsfz )
c     if (n .eq. 0) call exit(1)
C Write out temperature
      n = v5dwrite( 1, 7, vtz )
      if (n .eq. 0) call exit(1)
c     close the v5d file and exit
      n = v5dclose()
      if (n .eq. 0) call exit(1)
      if (n .ne. 0) return
      end
c ****************************************
      subroutine ckday(kday,kyr)
c
c --- This routine changes the Julian day from 365(6 if leap yr)
c --- to 1 and increases the year, if necessary.
c
      integer kday,kyr
      
      if (kday.eq.366) then
         if (mod(kyr,4).eq.0) then
            return
         else
            kday = 1
            kyr = kyr + 1
         endif
      else if (kday.eq.367) then
         kday = 1
         kyr = kyr + 1
      endif
      return
      end
c ****************************************
         integer function kgmt(imn,iday,iyr)

c --- This function computes the Julian day number (GMT) from the
c --- day, month, and year information.

         dimension mday(12)
         data mday /31,28,31,30,31,30,31,31,30,31,30,31/

         kgmt = 0
         ldays = 0
         leapyr = mod(iyr,4)
         if (leapyr.eq.0) then
            leapdy = 1
           else
            leapdy = 0
         endif

         if (imn.eq.1) goto 10

         do 20 i = 1,imn-1
            ldays = ldays + mday(i)
 20      continue

 10      if (leapdy.eq.1) then
             if (ldays.lt.59) leapdy = 0
         endif 

         kgmt = ldays + iday + leapdy
         return
         end
