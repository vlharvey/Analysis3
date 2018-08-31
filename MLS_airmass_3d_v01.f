c***************************************************************************
c  Program: MLS_airmass_3d                                              
c  reads UKMO,NMC or ECMWF  assimilated isentropic level data and computes 
c  diabatic trajectories using a fourth-order Runge-Kutta time step       
c                                                                        
c  Input data:           assimilated u,v,p,pv,q                         
c                        MLS mixing ratios                             
c  Input parameter file: MLS_airmass_3d_v05.fil                       
c  Output data:          trajectory x,y                                
c                                                                     
c  Programed by: R. Bradley Pierce 08-sep-1993                       
c                MS401B TSB                                         
c                NASA LARC                                         
c  Modularized by T. Duncan Fairlie February 1994                 
c  Updated by:   R.Bradley Pierce 02-oct-1996                      
c                MS401B TSB                                         
c                NASA LARC                                           
c                                                                     
c  use 12z winds for MLS airmass predictions.  read "merged" multiple
c  theta time-ordered MLS data on first day and increment to proper 
c  occultation (icount) based on the wind field being valid at 12Z.    
c  initialise multiple MLS airmasses per timestep.  new MLS data are 
c  read in once all previous have been initialized.                     
c                                                                        
c  Version 1 adapted from HIRDLS_airmass_3d_v01.f          VLH 12/23/05   
c                                                                          
c***************************************************************************

      include 'comm_mls_airmass_3d_v01.'
      REAL UVELOLD(NRmax,NCmax,nthwmx) 
      REAL VVELOLD(NRmax,NCmax,nthwmx) 
      REAL QVELOLD(NRmax,NCmax,nthwmx) 
      REAL POLD(NRmax,NCmax,nthwmx) 
      REAL PVOLD(NRmax,NCmax,nthwmx) 
      REAL QDFOLD(NRmax,NCmax,nthwmx) 
      REAL XMARKOLD(NRmax,NCmax,nthwmx) 
      REAL XMSFOLD(NRmax,NCmax,nthwmx)
      REAL VPOLD(NRmax,NCmax,nthwmx)
      REAL SFOLD(NRmax,NCmax,nthwmx)
C
      REAL UVELNEW(NRmax,NCmax,nthwmx) 
      REAL VVELNEW(NRmax,NCmax,nthwmx)
      REAL QVELNEW(NRmax,NCmax,nthwmx)
      REAL PNEW(NRmax,NCmax,nthwmx)
      REAL PVNEW(NRmax,NCmax,nthwmx)
      REAL QDFNEW(NRmax,NCmax,nthwmx)
      REAL XMARKNEW(NRmax,NCmax,nthwmx) 
      REAL XMSFNEW(NRmax,NCmax,nthwmx)
      REAL VPNEW(NRmax,NCmax,nthwmx)
      REAL SFNEW(NRmax,NCmax,nthwmx)
C
      real arr1(ncmax),arr2(nrmax),arr3(nthwmx)
      real arr4(nrmax,ncmax,nthwmx),arr5(nrmax,ncmax,nthwmx)
      real arr6(nrmax,ncmax,nthwmx),arr7(nrmax,ncmax,nthwmx)
      real arr8(nrmax,ncmax,nthwmx),arr9(nrmax,ncmax,nthwmx)
      real arr10(nrmax,ncmax,nthwmx),arr11(nrmax,ncmax,nthwmx)
      real arr12(nrmax,ncmax,nthwmx),arr13(nrmax,ncmax,nthwmx)
c
      character name*20
      integer*4 date, date2, hal_date 

      logical ex
      data ex /.false./
c
c  read/write input parameters for this experiment
c
      OPEN(5,FILE='MLS_airmass_3d_v01.fil',status='old')
      call read_list_header(5)
      call write_list_header(6)
c
c  truncate blank characters in input/output file directory
c
      lenw=0
      lenm=0
      do n=1,119
         if(dirw(n:n+1).ne.' ')lenw=lenw+1
         if(dirm(n:n+1).ne.' ')lenm=lenm+1
      enddo

      OPEN(12,FILE=OFILE,FORM='UNFORMATTED',STATUS='unknown')
c
c  write input parameters for this experiment to output file
c
      call write_unf_header(12)
C
c  restart from existing trajectory data set
c
      if (restart) then
        OPEN(13,FILE=dirw(1:lenw)//rFILE,
     &          FORM='UNFORMATTED',STATUS='old')
        call read_restart_data(13,etime)
        close(13)
      endif
c
c initialize constants and dependent time loop variables
c
      ystep=abs(60.*dtflow/dt)
      idt=dt
      idtflow=dtflow
      nstep=abs(60*idtflow/idt)
      dt=dt*60.
      dtb2 = abs(0.5*dt/3600.)         ! half timestep in hours
      PI2 = 6.2831853071796
      RAD=6.37E6
      ITFLAG=0 
      if ( .not. restart) then
        call zero_arrays
        etime=0.
        call jul_gmt
        ntraj=0
      endif
      wtime=0.0
      day=1
      icount=0
C
c loop over days
c
      DO 999 NFILE=1, NFILES
c
c open netCDF dataset w/o write privileges
c
         inquire(file=dirw(1:lenw)//IFILEw(NFILE),exist=ex)
         print *,dirw(1:lenw)//IFILEw(NFILE)
         if(ex)ncid=ncopn(dirw(1:lenw)//IFILEw(NFILE),NCNOWRIT,iret)
         if(.not.ex)stop
         call ncdinq(ncid, 1, name, nr,  iret)
         call ncdinq(ncid, 2, name, nc,  iret)
         call ncdinq(ncid, 3, name, nth, iret)
         nrw=nr
         ncw=nc
         nthw=nth
         call rdwind(ncid,arr1,arr2,arr3,arr4,arr5,
     1               arr6,arr7,arr8,arr9,arr10,arr11,
     2               arr12,arr13,nrw,ncw,nthw)

        DO 10 L = 1,NTH
         DO 10 J = 1,NR
          DO 10 I = 1,NC
            IF(ITFLAG.NE.0)THEN
              UVELOLD(J,I,L)=UVELNEW(J,I,L)
              VVELOLD(J,I,L)=VVELNEW(J,I,L)
              QVELOLD(J,I,L)=QVELNEW(J,I,L)
              POLD(J,I,L)=PNEW(J,I,L)
              PVOLD(J,I,L)=PVNEW(J,I,L)
              XMSFOLD(J,I,L)=XMSFNEW(J,I,L)
              QDFOLD(J,I,L)=QDFNEW(J,I,L)
              XMARKOLD(J,I,L)=XMARKNEW(J,I,L)
              VPOLD(J,I,L)=VPNEW(J,I,L)
              SFOLD(J,I,L)=SFNEW(J,I,L)
            ENDIF
            UVELNEW(J,I,L) = usim(J,I,L)
            VVELNEW(J,I,L) = vsim(J,I,L)
            QVELNEW(J,I,L) = qsim(J,I,L)
            XMSFNEW(J,I,L) = xmsfsim(J,I,L)
            PNEW(J,I,L) = psim(J,I,L)
            PVNEW(J,I,L) = pvsim(J,I,L)
            QDFNEW(J,I,L) = qdfsim(J,I,L)
            XMARKNEW(J,I,L) = xmarksim(J,I,L)
            VPNEW(J,I,L) = vpsim(J,I,L)
            SFNEW(J,I,L) = sfsim(J,I,L)
   10   CONTINUE
C
c read MLS occultations for first day and skip over first 
c or last 12 hours depending on forward or back trajectories
c to establish the counter "icount" at the proper place
c
        if(itflag.eq.0) then
           write(6,*)'opening first MLS file '//ifilem(nfile)
           OPEN(10,FILE=dirm(1:lenm)//IFILEm(NFILE),
     &          FORM='FORMATTED',status='old')
           call rdmls_merge(ncount,hal_date)
           close(10)
           print *,icount,ncount
           if (ncount.gt.0) then
               if (icount.eq.0) icount=1
   11          continue
               if( (dt.gt.0..AND.thal(icount).lt.12.).OR.       !forward
     +             (dt.lt.0..AND.thal(icount).gt.12.) )then     !back
c                  write(6,*)'skipping ',thal(icount)
                   icount=icount+1
                   goto 11
               endif
           endif
        endif
        IF(ITFLAG.EQ.0) GO TO 998	! end of first day
C
C loop over timesteps per day (NSTEP)
C
        DO 30 ISTEP=1,NSTEP
          XSTEP=ISTEP-1      
          TSCALE=(YSTEP-XSTEP)/YSTEP
          wmax=0.
          DO 20 L=1,NTH
          DO 20 I=1,NC
            DO 20 J=1,NR
              UGRD(J,I,L) = UVELNEW(J,I,L)+
     &          TSCALE*(UVELOLD(J,I,L)-UVELNEW(J,I,L))
              VGRD(J,I,L) = VVELNEW(J,I,L)+
     &          TSCALE*(VVELOLD(J,I,L)-VVELNEW(J,I,L))
              QGRD(J,I,L) = QVELNEW(J,I,L)+
     &          TSCALE*(QVELOLD(J,I,L)-QVELNEW(J,I,L))
              PGRD(J,I,L) = PNEW(J,I,L)+
     &          TSCALE*(POLD(J,I,L)-PNEW(J,I,L))
              PVGRD(J,I,L) = PVNEW(J,I,L)+
     &          TSCALE*(PVOLD(J,I,L)-PVNEW(J,I,L))
              XMSFGRD(J,I,L) = XMSFNEW(J,I,L)+
     &          TSCALE*(XMSFOLD(J,I,L)-XMSFNEW(J,I,L))
              QDFGRD(J,I,L) = QDFNEW(J,I,L)+
     &          TSCALE*(QDFOLD(J,I,L)-QDFNEW(J,I,L))
c ******************************************************
c do not interpolate marker field in time          VLH 5/1/02
c
              XMARKGRD(J,I,L) = XMARKNEW(J,I,L)
c ******************************************************
              VPGRD(J,I,L) = VPNEW(J,I,L)+
     &            TSCALE*(VPOLD(J,I,L)-VPNEW(J,I,L))
              SFGRD(J,I,L) = SFNEW(J,I,L)+
     &            TSCALE*(SFOLD(J,I,L)-SFNEW(J,I,L))
   20     CONTINUE
          wmax=sqrt(wmax)

          call calendar(istime,etime,ictime,ictime2)
          date=ictime/100
          date2=ictime2/100
c
c  check if it is time to initialize MLS airmasses- if they need to be
c
   22     continue
          if (ncount.gt.0 .and. icount.le.ncount) then
            if(icount.eq.0)icount=1
            tdiff=abs(abs(thal(icount))-abs(gmt))
            tdiff2=abs(abs(thal(icount))-abs(gmt2))
            if ( (date.eq.hal_date.and.tdiff.le.dtb2) .or.
     1           (date2.eq.hal_date.and.tdiff2.le.dtb2) ) then
               call inithal(icount)
               icount=icount+1
              goto 22
            endif
          endif
          print *,'time step ',istime,ictime,etime,icount,ncount,ntraj
c
c read more MLS data once all previous have been initialized
c
          if( (ncount.gt.0 .and. icount.gt.ncount) .or.
     &        (gmt.eq.0.) ) then
c    &        (ncount.eq.0 .and. istep.eq.1) ) then
            write(6,*)'opening MLS file '//ifilem(nfile)
            close(10)
            OPEN(10,FILE=dirm(1:lenm)//IFILEm(NFILE),
     &           FORM='FORMATTED',status='old')
            call rdmls_merge(ncount,hal_date)
            close(10)
            icount=0
   33     continue
          if (icount.lt.ncount) then
            if(icount.eq.0)icount=1
            tdiff=abs(abs(thal(icount))-abs(gmt))
            tdiff2=abs(abs(thal(icount))-abs(gmt2))
            if ( (date.eq.hal_date.and.tdiff.le.dtb2) .or.
     1           (date2.eq.hal_date.and.tdiff2.le.dtb2) ) then
               call inithal(icount)
              icount=icount+1
              goto 33
            endif
          endif
          endif
c
c get physical and dynamical diagnostics for chemistry 
c
          call getdiag

c compute solar zenith angle for each parcel and update
c fraction of time spent in daylight and nighttime          VLH 8/15/02
          call solar_fraction
c
c call diffusion
c
c         call diffusion
c
c kill trajectories older than "max_age" days
c
          if(ntraj.gt.0) call clean_house
c
c test for time to write to output file
c
          if((abs(wtime).lt.0.1).or.
     +       (abs(abs(wtime)-abs(dtout)).lt.0.1))then

              call write_output(12,etime)
              wtime=0.0
c
c refresh daylit and nighttime fractions, and min temp
c
              do itraj=1, ntraj
                 frday(itraj)=0.0
                 frnght(itraj)=0.0
                 xmint(itraj)=99999.
                 minttime(itraj)=99999
              enddo
          endif    ! end of write output condition
 
          CALL TRAJECT
 
          wtime=wtime+dt/3600.
          etime=etime+dt/3600.
          gmt = gmt + dt/3600.
          if (gmt .lt. 0.0) gmt=24.+gmt
          if (gmt .gt. 24.) gmt=gmt-24.
          if (abs(gmt).lt.0.1 .or.abs(gmt).gt.23.9) gmt=0.
          gmt2=gmt
          if(gmt2.eq.0.)gmt2=24.
          julday=julday+dt/(3600.*24.)
          if (julday .gt. 365) julday=julday-365
          if (julday .lt. 0) julday=365-julday
          call calendar(istime,etime,ictime,ictime2)
   30   CONTINUE
  998   ITFLAG=1
999   CONTINUE		
      STOP
      END 
      subroutine solar_fraction
c*****************************************************************c
c  Compute solar zenith angle for each parcel and update daylit   c
c  and nighttime fractions. frac_time is the fraction of an       c
c  output time interval in each timestep. julday (Julian day)     c
c  and gmt (Greenwich Mean Time) are continually updated          c
c                                     Duncan 4/10/97              c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      if (ntraj .ge. 1) then
        frac_time = abs(dt) / (3600.*dtout)
        doy=1.*julday
        do itraj=1, ntraj
          rlat=yn(itraj)
          rlon=xn(itraj)
          call zenith_angle(doy,gmt,rlat,rlon,chi)
          if(chi.le.90)then
            frday(itraj)=frday(itraj)+frac_time
          endif
          if(chi.gt.90)then
            frnght(itraj)=frnght(itraj)+frac_time
          endif
        enddo
      endif
      return
      end
      subroutine zenith_angle(doy,gmt,rlat,rlon,chi)
c*****************************************************************c
c  Compute solar zenith angle for each parcel                     c
c INPUT:                                                          c
c  doy   : julian day (INPUT)                                     c
c  gmt   : greenwich mean time (INPUT)                            c
c  tlat  : true latitude  (INPUT)                                 c
c  tlon  : true longitude (INPUT)                                 c
c OUTPUT:                                                         c
c  chi   : zenith angle (degrees) at position (tlat,tlon)         c
c*****************************************************************c
      pi=3.14159265
      dtor=pi/180.
      earinc=23.5
      sinlat=sin(rlat*dtor)
      coslat=sqrt(1.-sinlat**2)
      sinlon=sin(rlon*dtor)
      coslon=cos(rlon*dtor)
      soya=(doy-81.25)*pi/182.5           ! day angle
      soha=2.*pi*(gmt-12.)/24.            ! hour angle
      soha=-soha
      sininc=sin(earinc*dtor)
      sindec=sininc*sin(soya)
      cosdec= sqrt(1.-sindec**2)
      coszen=cos(soha)*coslon+sin(soha)*sinlon
      coszen=coszen*cosdec*coslat
      coszen=sindec*sinlat+coszen
      coszen=min(max(coszen,-1.),1.)
      chi = acos(coszen)
      chi = chi/dtor
      return
      end
      subroutine diffusion
c*****************************************************************c
c allow shear based diffusion to dilute chemical mixing ratios    c
c*****************************************************************c
      include "comm_mls_airmass_3d_v01."
c tscale is time scale in seconds
      parameter(tscale=5.184e6,qfac=8.0)
      integer idotraj(nmax)
      integer ipoint(nmax),mpoint(nmax)
c
c determine valid trajectories
c based on signal to noise ratio 
c 
       nvalid = 0
       do n=1,ntraj
          snhno3=hno3n_err(n)/hno3n(n)
          snxno2=xno2n_err(n)/xno2n(n)
          sno3=o3n_err(n)/o3n(n)
          snh2o=h2on_err(n)/h2on(n)
c       
         if ((snhno3.le.0.3).and.
     &       (snh2o.le.0.3).and.
     &       (snxno2.le.0.3).and.
     &       (sno3.le.0.3).and.
     &       (hno3n(n).ne.-999.).and.
     &       (h2on(n).ne.-999.).and.
     &       (xno2n(n).ne.-9999.).and.
     &       (o3n(n).ne.-9999.))then
           nvalid = nvalid+1
           idotraj(nvalid) = n
         endif
       enddo

      do 120 n=1,nvalid 
      np = idotraj(n)
      xp = xn(np)
      yp = yn(np)
      thp= thn(np)
      xmp = xmarkn(np)
      qdfp= qdfn(np)
c
c set diffusive tendencies to zero
c
        hno3nt(np)=0.
        h2ont(np)=0.
        xno2nt(np)=0.
        o3nt(np)=0.

      do l=1,nth-1
      if(thp.le.th(l).and.thp.gt.th(l+1))dth=th(l)-th(l+1)
      enddo
c
c find neighbors
      icount = 0
      do 100 nit=1,nvalid
        it = idotraj(nit)
        if ((abs(xp-xn(it)) .le. 2.*dlon) .and.
     +   (abs(yp-yn(it)) .le. 2.*dlat) .and.
     +   (abs(thp-thn(it)) .le. .5*dth)) then
          icount = icount+1
          ipoint(icount) = it
        endif
  100 continue
c
c find like neighbors (loop over neighboring trajectories)
      mcount = 0
      if(icount.gt.0) then 
      do 105 nit=1,icount
        it = ipoint(nit)
c        if (xmp .eq. xmarkn(it)) then
c          mcount = mcount+1
c          mpoint(mcount) = it
c        endif
c vortex parcels
        if((xmp.ge.0.9999).and.(xmarkn(it).ge.0.9999)) then
          mcount = mcount+1
          mpoint(mcount) = it
        endif
c anticyclone parcels
        if((xmp.le.-0.9999).and.(xmarkn(it).le.-0.9999)) then
          mcount = mcount+1
          mpoint(mcount) = it
        endif
c other parcels
        if(((xmp.gt.-0.9999).and.(xmp.lt.0.9999)).and.
     &     ((xmarkn(it).gt.-0.9999).and.
     &      (xmarkn(it).lt.0.9999)))then
          mcount = mcount+1
          mpoint(mcount) = it
        endif
  105 continue
      endif

c enhance mixing where qdf positive
      fac = 0.
c stabilize qdfp
      if (qdfp .gt. 0.) then
      qdfp=amin1(qdfp,qfac*log(tscale/dt -1.))
      fac=exp(qdfp/qfac)
      endif
c stabilize exponential fac
      fac = amin1(fac,tscale/dt-1.)
      timefac = 1./tscale
      timefacm = fac/tscale
c
c first compute background then enhanced diffusive tendency for 
c constituent arrays
c hno3n
      if(hno3n(np).ne.-9999.)then
        tendb=0.
        tende=0.
        avg=1.e24
        if(icount.gt.0)call sumit(icount,ipoint,hno3n,avg)
        if(avg.ne.1.e24)tendb = dt*(avg-hno3n(np))*timefac
        avg=1.e24
        if(mcount.gt.0)call sumit(mcount,mpoint,hno3n,avg)
        if(avg.ne.1.e24)tende = dt*(avg-hno3n(np))*timefacm
        hno3nt(np) = tendb+tende
      endif
c h2on
      if(h2on(np).ne.-9999.)then
        tendb=0.
        tende=0.
        avg=1.e24
        if(icount.gt.0)call sumit(icount,ipoint,h2on,avg)
        if(avg.ne.1.e24)tendb = dt*(avg-h2on(np))*timefac
        avg=1.e24
        if(mcount.gt.0)call sumit(mcount,mpoint,h2on,avg)
        if(avg.ne.1.e24)tende = dt*(avg-h2on(np))*timefacm
        h2ont(np) = tendb+tende
      endif
c xno2n
      if(xno2n(np).lt.1.e24)then
        tendb=0.
        tende=0.
        avg=1.e24
        if(icount.gt.0)call sumit(icount,ipoint,xno2n,avg)
        if(avg.ne.1.e24)tendb = dt*(avg-xno2n(np))*timefac
        avg=1.e24
        if(mcount.gt.0)call sumit(mcount,mpoint,xno2n,avg)
        if(avg.ne.1.e24)tende = dt*(avg-xno2n(np))*timefacm
        xno2nt(np) = tendb+tende
      endif
c o3n
      if(o3n(np).lt.1.e24)then
        tendb=0.
        tende=0.
        avg=1.e24
        if(icount.gt.0)call sumit(icount,ipoint,o3n,avg)
        if(avg.ne.1.e24)tendb = dt*(avg-o3n(np))*timefac
        avg=1.e24
        if(mcount.gt.0)call sumit(mcount,mpoint,o3n,avg)
        if(avg.ne.1.e24)tende = dt*(avg-o3n(np))*timefacm
        o3nt(np) = tendb+tende
      endif
c
  120 continue
      do 140 n=1,nvalid 
      np = idotraj(n)
c
c update mixing ratios with diffusive tendencies 
c
        hno3n(np)=hno3n(np)+hno3nt(np)
        h2on(np)=h2on(np)+h2ont(np)
        xno2n(np)=xno2n(np)+xno2nt(np)
        o3n(np)=o3n(np)+o3nt(np)
140   continue
c
      return
      end
      subroutine sumit(icount,point,mixn,avg)
c*****************************************************************c
c sum mixing ratio to obtain average                              c
c*****************************************************************c
      include "comm_mls_airmass_3d_v01."
      real mixn(nmax)
      integer point(nmax)
c
        sum=0.
        avg=1.e24
        npoint=icount
        do it=1,icount
         if(mixn(point(it)).lt.1.e24)sum = sum+mixn(point(it))
         if(mixn(point(it)).ge.1.e24)npoint=npoint-1
        enddo
c flag avg for case where all neighboring parcels are invalid
        if(npoint.gt.0)avg = sum/float(npoint)
        return
        end
      subroutine read_list_header(iunit)
c*****************************************************************c
c read from input file                                            c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      read(iunit,*)charexp                         
      read(iunit,*)ukmo                            
      read(iunit,*)nmc                             
      read(iunit,*)ecmwf                           
      read(iunit,*)restart
      read(iunit,*)rfile
      read(iunit,*)irtime
      read(iunit,*)dirw                             
      read(iunit,*)nfiles                          
      do n=1,nfiles                            
         read(iunit,*)ifilew(n)                        
      enddo                                    
      read(iunit,*)dirm                             
      read(iunit,*)nfiles                          
      do n=1,nfiles                            
         read(iunit,*)ifilem(n)                        
      enddo                                    
      read(iunit,*)istime
      read(iunit,*)dtflow                          
      read(iunit,*)dt                              
      read(iunit,*)igw                             
      read(iunit,*)dtout
      read(iunit,*)ofile
      return
      end
      subroutine write_list_header(iunit)
c*****************************************************************c
c write input file contents to the screen                         c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      write(iunit,*)charexp                         
      write(iunit,*)ukmo                            
      write(iunit,*)nmc                             
      write(iunit,*)ecmwf                           
      write(iunit,*)restart
      write(iunit,*)rfile
      write(iunit,*)irtime
      write(iunit,*)dirw                             
      write(iunit,*)nfiles                          
      do n=1,nfiles                            
         write(iunit,*)ifilew(n)                        
      enddo                                    
      write(iunit,*)dirm                             
      write(iunit,*)nfiles                          
      do n=1,nfiles                            
         write(iunit,*)ifilem(n)                        
      enddo               
      write(iunit,*)istime                     
      write(iunit,*)dtflow                          
      write(iunit,*)dt                              
      write(iunit,*)igw   
      write(iunit,*)dtout                          
      write(iunit,*)ofile
      return
      end
      subroutine write_unf_header(iunit)
c*****************************************************************c
c write input file contents to output file                        c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      write(iunit)charexp                         
      write(iunit)ukmo                            
      write(iunit)nmc                             
      write(iunit)ecmwf                           
      write(iunit)restart
      write(iunit)rfile
      write(iunit)irtime
      write(iunit)dirw                             
      write(iunit)nfiles                          
      do n=1,nfiles                            
         write(iunit)ifilew(n)                        
      enddo                                    
      write(iunit)dirm                             
      write(iunit)nfiles                          
      do n=1,nfiles                            
         write(iunit)ifilem(n)                        
      enddo             
      write(iunit)istime                       
      write(iunit)dtflow                          
      write(iunit)dt                              
      write(iunit)igw   
      write(iunit)dtout                          
      write(iunit)ofile
      return
      end
      subroutine read_restart_data(iunit,etime)
c*****************************************************************c
c read restart data from previous run                             c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      character*5 blank
c
c  skip over header information in restart unit
c
      do 10 k=1,9
10      read(iunit)
      read(iunit)nrf
      do n=1,nrf
        read(iunit)
      enddo                                    
      read(iunit)
      read(iunit)nrf
      do n=1,nrf
        read(iunit)
      enddo                                    
      do 20 k=1,6
20      read(iunit)
c
c skip to restart day
c
40    continue
      write(6,*)'reading restart data '
      READ(iunit) istime,ictime,etime,ntraj
      write(6,*)'data found for time',ictime
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(xn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(yn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(thn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(agen(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(x0n(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(y0n(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(th0n(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(t0n(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(z0n(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(p0n(j),j=1,ntraj)
      read(iunit)blank 
      write(6,*)blank
      READ(iunit)(hno3n(j),j=1,ntraj)
      read(iunit)blank 
      write(6,*)blank
      READ(iunit)(h2on(j),j=1,ntraj)
      read(iunit)blank 
      write(6,*)blank
      READ(iunit)(xno2n(j),j=1,ntraj)
      read(iunit)blank 
      write(6,*)blank
      READ(iunit)(o3n(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(hno3n_err(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(h2on_err(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(xno2n_err(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(o3n_err(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      read(iunit)blank 
      write(6,*)blank
      READ(iunit)(pvn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(pn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(xmsfn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(zn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(tmpn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(qn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(qdfn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(xmarkn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(frday(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(frnght(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(xmint(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(minttime(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(vpn(j),j=1,ntraj)
      read(iunit)blank
      write(6,*)blank
      READ(iunit)(sfn(j),j=1,ntraj)

      if (ictime .lt. irtime-1) goto 40
      if (ictime .gt. irtime+1) then 
          write(6,*)'ERROR: current data for time ',ictime
          write(6,*)'       exceeds restart time  ',irtime
          goto 40
      endif
      return
      end
      subroutine zero_arrays
c*****************************************************************c
c zero arrays at beginning of run                                 c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      do j = 1, nmax 
        xn(j)=99999.
        yn(j)=99999.
        thn(j)=99999.
        agen(j)=0.
        x0n(j)=99999.
        y0n(j)=99999.
        th0n(j)=99999.
        t0n(j)=99999.
        z0n(j)=99999.
        p0n(j)=99999.
        hno3n(j)=99999.
        h2on(j)=99999.
        xno2n(j)=99999.
        o3n(j)=99999.
        pvn(j)=99999.
        pn(j)=99999.
        xmsfn(j)=99999.
        zn(j)=99999.
        tmpn(j)=99999.
        qn(j)=99999.
        qdfn(j)=99999.
        xmarkn(j)=99999.
        frday(j)=0.0
        frnght(j)=0.0
        xmint(j)=99999.
        minttime(j)=99999
        vpn(j)=99999.
        sfn(j)=99999.
      enddo 
      return
      end
      subroutine clean_house 
c*****************************************************************c
c search through the trajectory arrays and throw out trajectories c
c older than "max_age" days. shunt the remaining trajectories to  c
c the beginning of the arrays          Duncan 10/4/99             c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      real*4 temp_array(nmax)
      data temp_array /nmax*99999./
      itraj = 1 
c
c scan through trajectories, checking ages (oldest ones come first). 
c
   10 continue
      if (abs(agen(itraj)).gt.max_age*24.)then
c       print *,'shunt ',itraj,agen(itraj)
        if (itraj.eq.ntraj) then 
            print *,'shunt all trajectories ',itraj,ntraj
            goto 20	! case of throwing out all VLH 3/28/02
        endif
        if (itraj.gt.ntraj) then
          print *,' error, shunt goes beyond ntraj ',itraj,ntraj,nmax
          stop
        endif 
        itraj=itraj+1
        goto 10
      endif 
   20 continue
      index = itraj 
c
c index is the index of the first parcel to be retained
c skip to the end if none of the parcels is too old 
c
      if (index .eq. 1) goto 100 
c
      write(6,30) index-1,agen(index)
   30 format (' THROWING OUT ',I6,'  PARCELS OLDER THAN ',
     1        f8.0,'  HOURS')
c
c check
c
c     do i=1,ntraj
c        print *,'before shunt ',i,agen(i)
c     enddo

      call shunt_trajec (xn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (yn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (zn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (agen,temp_array,index,ntraj,nmax) 
      do itraj=ntraj-index+2,ntraj
         agen(itraj)=0.
      enddo
      call shunt_trajec (hno3n,temp_array,index,ntraj,nmax) 
      call shunt_trajec (h2on,temp_array,index,ntraj,nmax) 
      call shunt_trajec (xno2n,temp_array,index,ntraj,nmax) 
      call shunt_trajec (o3n,temp_array,index,ntraj,nmax) 
      call shunt_trajec (xmarkn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (frday,temp_array,index,ntraj,nmax) 
      do itraj=ntraj-index+2,ntraj
         frday(itraj)=0.
      enddo
      call shunt_trajec (frnght,temp_array,index,ntraj,nmax) 
      do itraj=ntraj-index+2,ntraj
         frnght(itraj)=0.
      enddo
      call shunt_trajec (xmint,temp_array,index,ntraj,nmax) 
      call shunt_trajec (minttime,temp_array,index,ntraj,nmax)
      call shunt_trajec (vpn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (sfn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (pvn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (pn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (qdfn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (qn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (thn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (t0n,temp_array,index,ntraj,nmax) 
      call shunt_trajec (x0n,temp_array,index,ntraj,nmax) 
      call shunt_trajec (y0n,temp_array,index,ntraj,nmax) 
      call shunt_trajec (z0n,temp_array,index,ntraj,nmax) 
      call shunt_trajec (p0n,temp_array,index,ntraj,nmax) 
      call shunt_trajec (th0n,temp_array,index,ntraj,nmax) 
      call shunt_trajec (tmpn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (xmsfn,temp_array,index,ntraj,nmax) 
      call shunt_trajec (hno3n_err,temp_array,index,ntraj,nmax)
      call shunt_trajec (h2on_err,temp_array,index,ntraj,nmax)
      call shunt_trajec (xno2n_err,temp_array,index,ntraj,nmax)
      call shunt_trajec (o3n_err,temp_array,index,ntraj,nmax)
c
c reduce number of trajectories, ntraj, by (index-1) 
c
      if(index.ne.ntraj) ntraj = ntraj - index + 1  
      if(index.eq.ntraj) ntraj=0
      print *,'ntraj after shunt ',ntraj
c
c check
c
c     do i=1,ntraj 
c        print *,'after shunt ',i,agen(i)
c     enddo
c     stop

  100 continue 
      return
      end 
      subroutine shunt_trajec (array,temp,index,ntraj,nmax)
c*****************************************************************c
c discard the first (index-1) parcels and shunt the remaining     c
c trajectories to the start of the array.   Duncan 10/4/99        c
c*****************************************************************c
      real*4 array(nmax), temp(nmax)
c
c copy trajectories to be retained into temporary array, temp
c
      do itraj=index,ntraj
         itemp=itraj-index+1
         temp(itemp)=array(itraj)
c        print *,'moving',itraj,' to',itemp
      enddo
c
c overwrite trajectory array from the beginning
c
      do itraj=1,ntraj-index+1
         array(itraj)=temp(itraj)
c        print *,'new array ',itraj,index,ntraj
      enddo
c
c fill remaining slots with missing data indicators for safety
c
      do itraj=ntraj-index+2,ntraj
         array(itraj)=99999.
c        print *,'special for index ',itraj
      enddo
      return
      end
      subroutine write_output(iunit,etime)
c*****************************************************************c
c write trajectory output if there are trajectories
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      print *,'write output ',istime,ictime,etime,ntraj
      WRITE(iunit)istime,ictime,etime,ntraj

      if(ntraj.gt.0)then
      write(iunit)'xtraj'
      WRITE(iunit)(xn(j),j=1,ntraj)
      write(iunit)'ytraj'
      WRITE(iunit)(yn(j),j=1,ntraj)
      write(iunit)'thtrj'
      WRITE(iunit)(thn(j),j=1,ntraj)
      write(iunit)'agetr'
      WRITE(iunit)(agen(j),j=1,ntraj)
      write(iunit)'x0trj'
      WRITE(iunit)(x0n(j),j=1,ntraj)
      write(iunit)'y0trj'
      WRITE(iunit)(y0n(j),j=1,ntraj)
      write(iunit)'th0tr'
      WRITE(iunit)(th0n(j),j=1,ntraj)
      write(iunit)'t0trj'
      WRITE(iunit)(t0n(j),j=1,ntraj)
      write(iunit)'z0trj'
      WRITE(iunit)(z0n(j),j=1,ntraj)
      write(iunit)'p0trj'
      WRITE(iunit)(p0n(j),j=1,ntraj)
      write(iunit)'hno3trj'
      WRITE(iunit)(hno3n(j),j=1,ntraj)
      write(iunit)'h2otrj'
      WRITE(iunit)(h2on(j),j=1,ntraj)
      write(iunit)'xno2trj'
      WRITE(iunit)(xno2n(j),j=1,ntraj)
      write(iunit)'o3trj'
      WRITE(iunit)(o3n(j),j=1,ntraj)
      write(iunit)'hno3err'
      WRITE(iunit)(hno3n_err(j),j=1,ntraj)
      write(iunit)'h2oerr'
      WRITE(iunit)(h2on_err(j),j=1,ntraj)
      write(iunit)'xno2err'
      WRITE(iunit)(xno2n_err(j),j=1,ntraj)
      write(iunit)'o3err'
      WRITE(iunit)(o3n_err(j),j=1,ntraj)
      write(iunit)'pvtrj'
      WRITE(iunit)(pvn(j),j=1,ntraj)
      write(iunit)'ptraj'
      WRITE(iunit)(pn(j),j=1,ntraj)
      write(iunit)'msftr'
      WRITE(iunit)(xmsfn(j),j=1,ntraj)
      write(iunit)'ztraj'
      WRITE(iunit)(zn(j),j=1,ntraj)
      write(iunit)'tmptj'
      WRITE(iunit)(tmpn(j),j=1,ntraj)
      write(iunit)'qtraj'
      WRITE(iunit)(qn(j),j=1,ntraj)
      write(iunit)'qdftr'
      WRITE(iunit)(qdfn(j),j=1,ntraj)
      write(iunit)'mrktr'
      WRITE(iunit)(xmarkn(j),j=1,ntraj)
      write(iunit)'frday'
      WRITE(iunit)(frday(j),j=1,ntraj)
      write(iunit)'frnght'
      WRITE(iunit)(frnght(j),j=1,ntraj)
      write(iunit)'xmint'
      WRITE(iunit)(xmint(j),j=1,ntraj)
      write(iunit)'tmint'
      WRITE(iunit)(minttime(j),j=1,ntraj)
      write(iunit)'vptraj'
      WRITE(iunit)(vpn(j),j=1,ntraj)
      write(iunit)'sftraj'
      WRITE(iunit)(sfn(j),j=1,ntraj)
      endif
      return
      end
      subroutine jul_gmt
c*****************************************************************c
c  From the start time, istime, compute the Julian day and GMT    c
c  istime is in "yymmddhh" format.                                c
c                                             Duncan 8/13/99      c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'

      DIMENSION MNO(12)        
      integer*4 yy1,mm1,dd1,hh1
      data (MNO(I),I=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/    
      yy1=istime/1000000
      mm1=istime/10000-yy1*100
      dd1=istime/100-yy1*10000-mm1*100
      hh1=istime-yy1*1000000-mm1*10000-dd1*100
      julday = dd1
      if (mm1 .gt. 1) then
        do m = 1, mm1-1
          julday = julday + mno(m)
        end do
      endif
      gmt=1.0*hh1
      gmt2=gmt
      if(gmt.eq.0.)gmt2=24.
      return
      end
      subroutine calendar (istime,etime,ictime,ictime2)
c*****************************************************************c
c  Compute the current time, ictime, from the start time, istime, c
c       and the elapsed time in hours, etime.                     c
c  Both ictime and istime are integer*4 and expressed in the form c
c       "yymmddhh"                                                c
c*****************************************************************c
      DIMENSION MNO(12)        
      integer*4 istime,ictime
      integer*4 yy1,mm1,dd1,hh1,yy2,mm2,dd2,hh2
      data (MNO(I),I=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/    

      yy1=istime/1000000
      if (mod(yy1,4) .eq. 0.) mno(2)=29
      yy2=yy1
      mm1=istime/10000-yy1*100
      mm2=mm1
      dd1=istime/100-yy1*10000-mm1*100
      dd2=dd1+nint(etime)/24
      hh1=istime-yy1*1000000-mm1*10000-dd1*100

c --- Forward trajectories
      if (etime .gt. 0.) then
10        if (dd2 .gt. mno(mm1)) then
              dd2=dd2-mno(mm1)
              mm2=mm1+1
              if (mm2 .gt. 12) then
                  mm2=mm2-12
                  yy2=yy1+1
                  if (mod(yy2,4).eq.0.) mno(2)=29
                  if (mod(yy2,4).ne.0.) mno(2)=28
              endif
          endif
          do j=2,12
             if (dd2 .gt. mno(mm2)) then
                 dd2=dd2-mno(mm2)
                 mm2=mm1+j
                 if (mm2 .gt. 12) then
                     mm2=mm2-12
                     yy2=yy1+1
                     if (mod(yy2,4).eq.0.) mno(2)=29
                     if (mod(yy2,4).ne.0.) mno(2)=28
                 endif
             endif
          enddo
          if (dd2 .gt. mno(mm2)) goto 10

c --- Back trajectories
      elseif (etime .lt. 0.) then
20        if (dd2 .le. 0.) then
              mm2=mm1-1
              if (mm2 .eq. 0) then
                  mm2=mm2+12
                  yy2=yy1-1
                  if (mod(yy2,4).eq.0.) mno(2)=29
                  if (mod(yy2,4).ne.0.) mno(2)=28
              endif
              dd2=dd2+mno(mm2)
          endif
          do j=2,12
             if (dd2 .le. 0.) then
                 mm2=mm1-j
                 if (mm2 .le. 0) then
                     mm2=mm2+12
                     yy2=yy1-1
                     if (mod(yy2,4).eq.0.) mno(2)=29
                     if (mod(yy2,4).ne.0.) mno(2)=28
                 endif
                 dd2=dd2+mno(mm2)
             endif
          enddo
          if (dd2 .le. 0.) goto 20
      endif

c --- New hour
      hh2=mod(nint(etime),24)+hh1
      if(etime.gt.0.)then
         if(hh2.lt. 0) hh2=hh2+24
         if(hh2.ge.24)then
            hh2=hh2-24
            dd2=dd2+1
            if(dd2.gt.mno(mm2))then
               dd2=dd2-mno(mm2)
               mm2=mm2+1
               if(mm2.gt.12)then
                  mm2=mm2-12
                  yy2=yy2+1
               endif
            endif
         endif
      elseif(etime.lt.0.)then
         if(hh2.lt. 0)then
            hh2=hh2+24
            dd2=dd2-1
            if(dd2.lt.1)then
               mm2=mm2-1
               if (mm2.lt.1)mm2=12
               dd2=mno(mm2)
               if(mm2.eq.12)yy2=yy2-1
            endif
         endif
      endif

c --- Current time
c     print *,hh2,dd2,mm2,yy2
      ictime=hh2+dd2*100+mm2*10000+yy2*1000000

c --- current times alter ego, i.e. 2003010100=2002123124
      ictime2=ictime
      if(hh2.ne.0)return
c
c previous day
c
      if(dd2.gt.1.)then
        ictime2=24+(dd2-1)*100+mm2*10000+yy2*1000000
        return
      endif
c
c last day of previous month
c
      if(dd2.eq.1.AND.mm2.gt.1)then
         mm2=mm2-1
         dd2=mno(mm2)
         ictime2=24+dd2*100+mm2*10000+yy2*1000000
         return
      endif
c
c last day of previous year
c
      if(dd2.eq.1.AND.mm2.eq.1)then
         yy2=yy2-1
         mm2=12
         dd2=mno(mm2)
         ictime2=24+dd2*100+mm2*10000+yy2*1000000
         return
      endif
      end
      subroutine rdwind(ncid,arr1,arr2,arr3,arr4,arr5,
     1                  arr6,arr7,arr8,arr9,arr10,arr11,
     2                  arr12,arr13,nrw,ncw,nthw)
c*****************************************************************c
c   Read flow field data                                          c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      integer istart(3),kcount(3)
c
      dimension arr1(ncw),arr2(nrw),arr3(nthw)
      dimension arr4(nrw,ncw,nthw),arr5(nrw,ncw,nthw),
     &          arr6(nrw,ncw,nthw),arr7(nrw,ncw,nthw),
     &          arr8(nrw,ncw,nthw),arr9(nrw,ncw,nthw),
     &          arr10(nrw,ncw,nthw),arr11(nrw,ncw,nthw),
     &          arr12(nrw,ncw,nthw),arr13(nrw,ncw,nthw)
      data istart/1,1,1/
      kcount(1)=nrw
      kcount(2)=ncw
      kcount(3)=nthw
c
c --- 1=lons, 2=lats, 3=thetas
c
      call ncvgt(ncid, 1, 1, ncw, arr1, iret)
      call ncvgt(ncid, 2, 1, nrw, arr2, iret)
      call ncvgt(ncid, 3, 1, nthw,arr3, iret)
      call ncvgt(ncid, 4, istart, kcount, arr4, iret)
      call ncvgt(ncid, 5, istart, kcount, arr5, iret)
      call ncvgt(ncid, 6, istart, kcount, arr6, iret)
      call ncvgt(ncid, 7, istart, kcount, arr7, iret)
      call ncvgt(ncid, 8, istart, kcount, arr8, iret)
      call ncvgt(ncid, 9, istart, kcount, arr9, iret)
      call ncvgt(ncid,10, istart, kcount, arr10, iret)
      call ncvgt(ncid,11, istart, kcount, arr11, iret)
      call ncvgt(ncid,12, istart, kcount, arr12, iret)
      call ncvgt(ncid,13, istart, kcount, arr13, iret)
      call ncclos(ncid, iret)
c
      dlon=arr1(2)-arr1(1)
      dlat=arr2(2)-arr2(1)
      do i=1,nc
        x(i)=arr1(i)
      enddo
      do j=1,nr
        y(j)=arr2(j)
      enddo
      do l=1,nth
        th(l)=arr3(l)
      enddo
      do i=1,nc
        do j=1,nr
          do l=1,nth
            pvsim(j,i,l)=arr4(j,i,l)
             psim(j,i,l)=arr5(j,i,l)
          xmsfsim(j,i,l)=arr6(j,i,l)
C regular velocity components                    Duncan  10/20/99
             usim(j,i,l)=arr7(j,i,l)
             vsim(j,i,l)=arr8(j,i,l)
C convert heating rate from deg/day to deg/sec   Duncan  7/14/99
             qsim(j,i,l)=arr9(j,i,l)/86400.
c QDF deformation diagnostic included            Duncan  7/14/99
           qdfsim(j,i,l)=arr10(j,i,l)
c Vortex Marker included (-1=AC, 0=none, +1=CY)  Duncan  7/14/99
c            xmarksim(j,i,l)=arr11(j,i,l)
c ensure values between -1 and 1                VLH 3/12/03
             if(arr11(j,i,l).ne.0.)then
                xmarksim(j,i,l)=arr11(j,i,l)/abs(arr11(j,i,l))
             endif
             vpsim(j,i,l)=arr12(j,i,l)
             sfsim(j,i,l)=arr13(j,i,l)
          enddo
        enddo
      enddo
      return
      end
      subroutine rdmls_merge(ncount,hal_date)
c*****************************************************************c
c read merged MLS data
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
c
      integer*4 ncount
      integer*4 hal_date
      integer*4 i13
      real*4 a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12
      real*4 b1,b2,b3,b4
      real*4 e1,e2,e3,e4
C
      read(10,*)hal_date
      print *,' hal_date = ',hal_date
      read(10,*)ncount
      print *,' # occult = ',ncount
      if (ncount .eq. 0) then
        print *,'no occultations found in mls file'
        goto 100
      endif
      do n = 1, ncount
c
c printf,4,t,y,x,th,o3,eo3
c
         read(10,*)a1,a2,a3,a4,a5,a6	!,a7,a8,a9,a10,a11,a12,i13
         thal (n) = a1
         yhal (n) = a2		! latitude
         xhal (n) = a3		! longitude
         thhal(n) = a4		! potential temperature (K)
c        xsat (n) = a5		! mls satellite longitude
c        ysat (n) = a6		! mls satellite latitude
c        phal (n) = a7		! pressure (mbar)
c        zhal (n) = a8		! altitude (km)
c        ptrophal (n) = a9	! tropopause pressure
c        ztrophal (n) = a10	! tropopause height
c        thtrophal(n) = a11	! tropopause theta
c        cloudhal (n) = a12	! no cloud flag for mls
c        modehal  (n) = i13	! sunrise=0 sunset=1

c hno3dat,no2dat,o3dat,h2odat
c        read(10,*)b1,b2,b3,b4
c        hno3(n)=b1
c        xno2(n)=b2
c        o3(n)=b3
         o3(n)=a5
c        h2o(n)=b4

c hno3errdat,no2errdat,o3errdat,h2oerrdat
c        read(10,*)e1,e2,e3,e4
c        hno3_err(n)=e1
c        xno2_err(n)=e2
c        o3_err(n)=e3
         o3_err(n)=a6
c        h2o_err(n)=e4
      enddo
100   continue
      close(10)
      return
      end
      subroutine getdiag
c*****************************************************************c
c interpolate dynamical diagnostics to the trajectory locations   c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      do 80 n = 1, ntraj
        xp=xn(n)
        yp=yn(n)
        zp=thn(n)
        if(xp.lt.x(1))xp=xp+360.
        do 40 l=1,nth-1
        lp1=l+1
        thl=th(l) 
        thlp1=th(lp1)
c VLH 8/14/02         if((zp.le.thl).and.(zp.gt.thlp1))then
        if((zp.le.thl).and.(zp.ge.thlp1))then
        zscale=(zp-thl)/(thlp1-thl)
        do 30 i=1,nc
        ip1=i+1
        if(i.eq.nc)ip1=1
        xlon=x(i) 
        xlonp1=x(ip1)
        if(i.eq.nc)xlonp1=360.+x(ip1)
c VLH 8/14/02            if((xp.ge.xlon).and.(xp.lt.xlonp1))then
        if((xp.ge.xlon).and.(xp.le.xlonp1))then
        xscale=(xp-xlon)/(xlonp1-xlon)
        do 20 j=1,nr-1
        jp1=j+1
        xlat=y(j)
        xlatp1=y(jp1)
        if((yp.ge.xlat).and.(yp.le.xlatp1))then
        yscale=(yp-xlat)/(xlatp1-xlat)
c
        pvj1=pvgrd(j,i,l)+xscale*(pvgrd(j,ip1,l)-pvgrd(j,i,l))
        pj1=pgrd(j,i,l)+xscale*(pgrd(j,ip1,l)-pgrd(j,i,l))
        xmsfj1=xmsfgrd(j,i,l)+xscale*(xmsfgrd(j,ip1,l)-xmsfgrd(j,i,l))
        qj1=qgrd(j,i,l)+xscale*(qgrd(j,ip1,l)-qgrd(j,i,l))
        qdfj1=qdfgrd(j,i,l)+xscale*(qdfgrd(j,ip1,l)-qdfgrd(j,i,l))
        vpj1=vpgrd(j,i,l)+xscale*(vpgrd(j,ip1,l)-vpgrd(j,i,l))
        sfj1=sfgrd(j,i,l)+xscale*(sfgrd(j,ip1,l)-sfgrd(j,i,l))
        xmrkj1=xmarkgrd(j,i,l)
     +        +xscale*(xmarkgrd(j,ip1,l)-xmarkgrd(j,i,l))
c
        pvjp1=pvgrd(jp1,i,l)+xscale*(pvgrd(jp1,ip1,l)-pvgrd(jp1,i,l))
        pjp1=pgrd(jp1,i,l)+xscale*(pgrd(jp1,ip1,l)-pgrd(jp1,i,l))
        xmsfjp1=xmsfgrd(jp1,i,l)
     +        +xscale*(xmsfgrd(jp1,ip1,l)-xmsfgrd(jp1,i,l))
        qjp1=qgrd(jp1,i,l)+xscale*(qgrd(jp1,ip1,l)-qgrd(jp1,i,l))
        qdfjp1=qdfgrd(jp1,i,l)
     +        +xscale*(qdfgrd(jp1,ip1,l)-qdfgrd(jp1,i,l))
        vpjp1=vpgrd(jp1,i,l)
     +        +xscale*(vpgrd(jp1,ip1,l)-vpgrd(jp1,i,l))
        sfjp1=sfgrd(jp1,i,l)
     +        +xscale*(sfgrd(jp1,ip1,l)-sfgrd(jp1,i,l))
        xmrkjp1=xmarkgrd(jp1,i,l)
     +        +xscale*(xmarkgrd(jp1,ip1,l)-xmarkgrd(jp1,i,l))
c
        pvj2=pvgrd(j,i,lp1)+xscale*(pvgrd(j,ip1,lp1)-pvgrd(j,i,lp1))
        pj2=pgrd(j,i,lp1)+xscale*(pgrd(j,ip1,lp1)-pgrd(j,i,lp1))
        xmsfj2=xmsfgrd(j,i,lp1)
     +       +xscale*(xmsfgrd(j,ip1,lp1)-xmsfgrd(j,i,lp1))
        qj2=qgrd(j,i,lp1)+xscale*(qgrd(j,ip1,lp1)-qgrd(j,i,lp1))
        qdfj2=qdfgrd(j,i,lp1)
     +       +xscale*(qdfgrd(j,ip1,lp1)-qdfgrd(j,i,lp1))
        vpj2=vpgrd(j,i,lp1)
     +       +xscale*(vpgrd(j,ip1,lp1)-vpgrd(j,i,lp1))
        sfj2=sfgrd(j,i,lp1)
     +       +xscale*(sfgrd(j,ip1,lp1)-sfgrd(j,i,lp1))
        xmrkj2=xmarkgrd(j,i,lp1)
     +       +xscale*(xmarkgrd(j,ip1,lp1)-xmarkgrd(j,i,lp1))
c
        pvjp2=pvgrd(jp1,i,lp1)+
     &        xscale*(pvgrd(jp1,ip1,lp1)-pvgrd(jp1,i,lp1))
        pjp2=pgrd(jp1,i,lp1)+xscale*(pgrd(jp1,ip1,lp1)-pgrd(jp1,i,lp1))
        xmsfjp2=xmsfgrd(jp1,i,lp1)+
     &        xscale*(xmsfgrd(jp1,ip1,lp1)-xmsfgrd(jp1,i,lp1))
        qjp2=qgrd(jp1,i,lp1)+xscale*(qgrd(jp1,ip1,lp1)-qgrd(jp1,i,lp1))
        qdfjp2=qdfgrd(jp1,i,lp1)+
     &        xscale*(qdfgrd(jp1,ip1,lp1)-qdfgrd(jp1,i,lp1))
        vpjp2=vpgrd(jp1,i,lp1)+
     +        xscale*(vpgrd(jp1,ip1,lp1)-vpgrd(jp1,i,lp1))
        sfjp2=sfgrd(jp1,i,lp1)+
     +        xscale*(sfgrd(jp1,ip1,lp1)-sfgrd(jp1,i,lp1))
        xmrkjp2=xmarkgrd(jp1,i,lp1)+
     +        xscale*(xmarkgrd(jp1,ip1,lp1)-xmarkgrd(jp1,i,lp1))
c
        pv1=pvj1+yscale*(pvjp1-pvj1)
        p1=pj1+yscale*(pjp1-pj1)
        xmsf1=xmsfj1+yscale*(xmsfjp1-xmsfj1)
        q1=qj1+yscale*(qjp1-qj1)
        qdf1=qdfj1+yscale*(qdfjp1-qdfj1)
        vp1=vpj1+yscale*(vpjp1-vpj1)
        sf1=sfj1+yscale*(sfjp1-sfj1)
        xmrk1=xmrkj1+yscale*(xmrkjp1-xmrkj1)
c
        pv2=pvj2+yscale*(pvjp2-pvj2)
        p2=pj2+yscale*(pjp2-pj2)
        xmsf2=xmsfj2+yscale*(xmsfjp2-xmsfj2)
        q2=qj2+yscale*(qjp2-qj2)
        qdf2=qdfj2+yscale*(qdfjp2-qdfj2)
        vp2=vpj2+yscale*(vpjp2-vpj2)
        sf2=sfj2+yscale*(sfjp2-sfj2)
        xmrk2=xmrkj2+yscale*(xmrkjp2-xmrkj2)
c
        pvn(n)=pv1+zscale*(pv2-pv1)
        pn(n)=p1+zscale*(p2-p1)
        xmsfn(n)=xmsf1+zscale*(xmsf2-xmsf1)
        qn(n)=q1+zscale*(q2-q1)
        qdfn(n)=qdf1+zscale*(qdf2-qdf1)
        vpn(n)=vp1+zscale*(vp2-vp1)
        sfn(n)=sf1+zscale*(sf2-sf1)
        xmarkn(n)=xmrk1+zscale*(xmrk2-xmrk1)
c
c need temperature to calculate zn
c
        tmpn(n)=thn(n)*(pn(n)/1000.0)**0.286
c
c update minimum temperature                            VLH 3/15/2003
c
        if (tmpn(n) .lt. xmint(n)) then
            xmint(n) = tmpn(n)
            minttime(n)= ictime
        endif
c
c height of trajectory (km)
c
        zn(n)= (xmsfn(n) - 1004.0*tmpn(n))/(9.8*1000.0)
c
c if trajectory is underground
c
        if (pvgrd(j,ip1,l).eq.1.e12.or.pvgrd(j,i,l).eq.1.e12.or.
     +      pvgrd(jp1,i,l).eq.1.e12.or.pvgrd(jp1,ip1,l).eq.1.e12)
     +      zn(n)=0.0
c
c ********************************************************
c do not interpolate marker field in space.  assign the
c value that the majority of the vertices have.  In case
c of a tie set to zero        VLH 5/1/02
c
c       xmarkv1=xmarkgrd(j,i,l)
c       xmarkv2=xmarkgrd(j,ip1,l)
c       xmarkv3=xmarkgrd(jp1,i,l)
c       xmarkv4=xmarkgrd(jp1,ip1,l)
c       xmarkv5=xmarkgrd(j,i,lp1)
c       xmarkv6=xmarkgrd(j,ip1,lp1)
c       xmarkv7=xmarkgrd(jp1,i,lp1)
c       xmarkv8=xmarkgrd(jp1,ip1,lp1)
c       xmarkvt=xmarkv1+xmarkv2+xmarkv3+xmarkv4+
c    $          xmarkv5+xmarkv6+xmarkv7+xmarkv8
c       if(xmarkvt.eq.0)xmarkn(n)=0.
c       if(xmarkvt.gt.0)xmarkn(n)=1.
c       if(xmarkvt.lt.0)xmarkn(n)=-1.
c ********************************************************
        goto 50
        endif
20      continue
        endif
30      continue
        endif
40      continue
50      continue
80    continue
      return
      end  
      subroutine traject
c*****************************************************************c
c compute new trajectory position using Runge-Kutta timestepping  c
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      real m1,n1,l1
      real m2,n2,l2
      real m3,n3,l3
      real m4,n4,l4
      PI2 = 6.2831853071796
      RAD=6.37E6
      radg=pi2/360.
      pi=.5*pi2
      do 100 n=1,ntraj
      call interp(u,v,q,uigw,vigw,xn(n),yn(n),thn(n))
      m1=dt*(180./pi)*u/(rad*cos(radg*yn(n)))
      n1=dt*(180./pi)*v/rad
      l1=dt*q
      xprm = xn(n) + .5*m1
      yprm = yn(n) + .5*n1
      zprm = thn(n) + .5*l1
      call interp(u,v,q,uigw,vigw,xprm,yprm,zprm)
      m2=dt*(180./pi)*u/(rad*cos(radg*yprm))
      n2=dt*(180./pi)*v/rad
      l2=dt*q
      xprm = xn(n) + .5*m2
      yprm = yn(n) + .5*n2
      zprm = thn(n) + .5*l2
      call interp(u,v,q,uigw,vigw,xprm,yprm,zprm)
      m3=dt*(180./pi)*u/(rad*cos(radg*yprm))
      n3=dt*(180./pi)*v/rad
      l3=dt*q
      xprm = xn(n) + m3
      yprm = yn(n) + n3
      zprm = thn(n) + l3
      call interp(u,v,q,uigw,vigw,xprm,yprm,zprm)
      m4=dt*(180./pi)*u/(rad*cos(radg*yprm))
      n4=dt*(180./pi)*v/rad
      l4=dt*q
      xn(n) = xn(n) + (m1+2.*m2+2.*m3+m4)/6.
      yn(n) = yn(n) + (n1+2.*n2+2.*n3+n4)/6.
      thn(n) = thn(n) + (l1+2.*l2+2.*l3+l4)/6.
      agen(n) = agen(n) + dt/3600.
c
c cross pole adjustment
c
      if (yn(n).lt.-90.) xn(n)=  180.+xn(n)
      if (yn(n).lt.-90.) yn(n)= -180.-yn(n)
      if (yn(n).gt. 90.) xn(n)=  180.+xn(n)
      if (yn(n).gt. 90.) yn(n)=  180.-yn(n)
      if (xn(n).lt.  0.) xn(n)=  360.+xn(n)
      if (xn(n).ge.360.) xn(n)= -360.+xn(n)
100   continue
      return
      end
      subroutine inithal(icount)
c*****************************************************************c
c Initialize clustered MLS airmasses using the angle
c toward the satellite instead of the satellite lon,lat
c*****************************************************************c
      include 'comm_mls_airmass_3d_v01.'
      PI2 = 6.2831853071796
      dtr = 180./(pi2*0.5)
      RAD=6.37E6
      RADG = PI2 / 360.
c
c tangent path length of 200 km
c
      s=100.e3
      ds=s/nclus
      n=ntraj+1

      if(yhal(icount).eq.90.)then
         sina=1.0e-10
         cosa=sqrt(1.-sina**2)
      endif
      if(yhal(icount).eq.-90.)then
         sina=1.0e-10
         cosa=-1.0*sqrt(1.-sina**2)
      endif
      if(abs(yhal(icount)).lt.90.)then
         a=(90.-yhal(icount))/dtr
         sina=sin(a)
         cosa=cos(a)
      endif
c
c  center of the tangent path (s)
c
      olon=xhal(icount)
      olat=yhal(icount)
      xn   (n) = olon
      yn   (n) = olat
      thn  (n) = thhal(icount)
      agen (n) = 0.0
      frday(n) = 0.0
      frnght(n)= 0.0
      x0n  (n) = olon
      y0n  (n) = olat
      th0n (n) = thhal(icount)
      t0n  (n) = thal (icount)
      z0n  (n) = zhal (icount)
      p0n  (n) = phal (icount)
      hno3n (n)=hno3 (icount)
      h2on (n)=h2o (icount)
      xno2n(n)=xno2(icount)
      o3n  (n)=o3  (icount)
      hno3n_err (n)=hno3_err (icount)
      h2on_err (n)=h2o_err (icount)
      xno2n_err(n)=xno2_err(icount)
      o3n_err  (n)=o3_err  (icount)
      ntraj=ntraj+nclus
      return
      END
	subroutine interp(u,v,q,uigw,vigw,xp,yp,zp)
c*****************************************************************c
c interpolate winds to trajectory positions                       c
c*****************************************************************c
	include 'comm_mls_airmass_3d_v01.'
        uigw=0.
        vigw=0.
        u=0.
        v=0.
        q=0.
        xpold=xp
        ypold=yp
        zpold=zp
        if(xp.lt.x(1))xp=xp+360.
c
c pole points
c
        lsp=0
        lnp=0
        if(yp.lt.y(1))lsp=1
        if(yp.lt.y(1))yp=y(1)
        if(yp.gt.y(nr))lnp=1
        if(yp.gt.y(nr))yp=y(nr)
c
c top/bottom points
c
        lup=0
        ldn=0
        if(zp.gt.th(1))lup=1
        if(zp.gt.th(1))zp=th(1)
        if(zp.lt.th(nth))ldn=1
        if(zp.lt.th(nth))zp=th(nth)
        do 40 l=1,nth-1
        lp1=l+1
        thl=th(l) 
        thlp1=th(lp1)
        if((zp.le.thl).and.(zp.ge.thlp1))then
        zscale=(zp-thl)/(thlp1-thl)
        do 30 i=1,nc
        ip1=i+1
        if(i.eq.nc)ip1=1
        xlon=x(i) 
        xlonp1=x(ip1)
        if(i.eq.nc)xlonp1=360.+x(ip1)
        if((xp.ge.xlon).and.(xp.lt.xlonp1))then
        xscale=(xp-xlon)/(xlonp1-xlon)
        do 20 j=1,nr-1
        jp1=j+1
        xlat=y(j)
        xlatp1=y(jp1)
        if((yp.ge.xlat).and.(yp.le.xlatp1))then
        yscale=(yp-xlat)/(xlatp1-xlat)
c
        uj1=ugrd(j,i,l)+xscale*(ugrd(j,ip1,l)-ugrd(j,i,l))
        vj1=vgrd(j,i,l)+xscale*(vgrd(j,ip1,l)-vgrd(j,i,l))
        qj1=qgrd(j,i,l)+xscale*(qgrd(j,ip1,l)-qgrd(j,i,l))
c
        ujp1=ugrd(jp1,i,l)+xscale*(ugrd(jp1,ip1,l)-ugrd(jp1,i,l))
        vjp1=vgrd(jp1,i,l)+xscale*(vgrd(jp1,ip1,l)-vgrd(jp1,i,l))
        qjp1=qgrd(jp1,i,l)+xscale*(qgrd(jp1,ip1,l)-qgrd(jp1,i,l))
c
        uj2=ugrd(j,i,lp1)+xscale*(ugrd(j,ip1,lp1)-ugrd(j,i,lp1))
        vj2=vgrd(j,i,lp1)+xscale*(vgrd(j,ip1,lp1)-vgrd(j,i,lp1))
        qj2=qgrd(j,i,lp1)+xscale*(qgrd(j,ip1,lp1)-qgrd(j,i,lp1))
c
        ujp2=ugrd(jp1,i,lp1)+xscale*(ugrd(jp1,ip1,lp1)-ugrd(jp1,i,lp1))
        vjp2=vgrd(jp1,i,lp1)+xscale*(vgrd(jp1,ip1,lp1)-vgrd(jp1,i,lp1))
        qjp2=qgrd(jp1,i,lp1)+xscale*(qgrd(jp1,ip1,lp1)-qgrd(jp1,i,lp1))
c
        u1=uj1+yscale*(ujp1-uj1)
        v1=vj1+yscale*(vjp1-vj1)
        q1=qj1+yscale*(qjp1-qj1)
c
        u2=uj2+yscale*(ujp2-uj2)
        v2=vj2+yscale*(vjp2-vj2)
        q2=qj2+yscale*(qjp2-qj2)
c
        u=u1+zscale*(u2-u1)
        v=v1+zscale*(v2-v1)
        q=q1+zscale*(q2-q1)
        wmag=sqrt(u**2+v**2)
        if(igw) then 
        if(wmag.ge.60.) then
        call igwave(uigw,vigw,u,v,i,j,xp,yp,wmag) 
        u=u+uigw
        v=v+vigw
        endif
        endif
        goto 50
        endif
20      continue
        endif
30      continue
        endif
40      continue
50      continue
        xp=xpold
        yp=ypold
c
c top/bottom points
c
        if((lup.ne.1).and.(ldn.ne.1))zp=zpold
        if((lup.eq.1).or.(ldn.eq.1))q=0.
c
c pole points
c
        if((lsp.eq.1).or.(lnp.eq.1))u=0.
	return
	end  
        subroutine igwave(uigw,vigw,u,v,i,j,xp,yp,wmag) 
c*****************************************************************c
c generate internal gravity wave                                  c
c*****************************************************************c
        include 'comm_mls_airmass_3d_v01.'
        real m,l,k,n2,f,r,omega,horz,pi,radg
        PI2 = 6.2831853071796
        RAD=6.37E6
        radg=pi2/360.
        pi=.5*pi2
        t=gmt*3600.
c
c major axis of elliptically polarized IGW oriented in direction of jet max
c
        if(v/wmag.gt.1.)v=wmag
        if(v/wmag.lt.-1.)v=-wmag
	theta1=acos(v/wmag)
	if((v.gt.0.).and.(u.lt.0.)) 
     &  theta1=2.*pi-theta1
	if((v.lt.0.).and.(u.lt.0.)) 
     &  theta1=theta1+pi/2.
c
c major axis of elliptically polarized IGW oriented 45 degrees clockwise on
c cyclonic side of jet max and 45 degrees counter clockwise on anticyclonic 
c side of jet max (like a bow wave)
c
        jp1=j+1
        jm1=j-1
        ip1=i+1
        im1=i-1
        if(i.eq.nc)ip1=1
        if(i.eq.1)im1=nc
        if(j.eq.nr)jp1=j
        if(j.eq.1)jm1=1
        dudy=ugrd(jm1,i,2)-ugrd(jp1,i,2)
        dvdx=vgrd(j,ip1,2)-vgrd(j,im1,2)
        goto 10
	if((u.ge.0.).and.(v.ge.0.)) then 
	if((dvdx.le.0.).and.(dudy.ge.0.)) theta1=theta1+pi/4. 
	if((dvdx.ge.0.).and.(dudy.le.0.)) theta1=theta1-pi/4. 
	goto 10
	endif
	if((u.ge.0.).and.(v.le.0.)) then 
	if((dvdx.le.0.).and.(dudy.ge.0.)) theta1=theta1+pi/4. 
	if((dvdx.ge.0.).and.(dudy.le.0.)) theta1=theta1-pi/4. 
	goto 10
	endif
	if((u.le.0.).and.(v.le.0.)) then 
	if((dvdx.le.0.).and.(dudy.ge.0.)) theta1=theta1+pi/4. 
	if((dvdx.ge.0.).and.(dudy.le.0.)) theta1=theta1-pi/4. 
	goto 10
	endif
	if((u.le.0.).and.(v.ge.0.)) then 
	if((dvdx.le.0.).and.(dudy.ge.0.)) theta1=theta1+pi/4. 
	if((dvdx.ge.0.).and.(dudy.le.0.)) theta1=theta1-pi/4. 
	goto 10
	endif
10      continue
c
c vertical wavelength (m-1) 
c
	m=1./3.e3                          
c
c meridional wavelength (m-1)
c
	l=1./100.e3                        
c
c longitudinal wavelength (m-1) 
c
	k=1./100.e3                        
c
c B-V frequency squared (s-2)
c
	n2=9.8*(log(1500.)-log(850.))/10e3  
c
c coriolis (s-1)
c
	f=2.*7.292e-5*sin(y(j)*radg)           
c
c gas constant 
c
	r=287.                             
c
c 6 hr inertial gravity wave frequency 
c
	omega=-1./(6.*3600.)
	horz=((2.*pi*omega)**2-f**2)*m**2/n2
	k=sin(theta1)*sqrt(horz)
	l=cos(theta1)*sqrt(horz)
c
c temperature perturbation
c                              
c	temp0=3.*exp(-(wmax-wmag)**2/(wmax-70.)**2)
c
c temperature perturbation
c                              
c	temp0=1.*exp(-(wmax-wmag)**2/(wmax-70.)**2)
c
c temperature perturbation
c                              
	temp0=3.*exp(-(wmax-wmag)**2/(wmax-70.)**2)
        dx=xp-x(i)
        dy=yp-y(j)
        if(dx.gt.360./96.) dx=dx-360.
        if(dx.lt.-360./96.) dx=dx+360.
	x1=rad*dx*radg*cos(y(j)*radg)
	y1=rad*dy*radg
c
c  inertial gravity wave
c
	temp=sin(2.*pi*(k*x1+l*y1-
     &           (omega+k*u+l*v)*t))+
     &       cos(2.*pi*(k*x1+l*y1-
     &           (omega+k*u+l*v)*t))
	uigw=k*omega*sin(2.*pi*(k*x1+l*y1-
     &                   (omega+k*u+l*v)*t))-
     &           l*f*cos(2.*pi*(k*x1+l*y1-
     &                   (omega+k*u+l*v)*t))
	vigw=l*omega*sin(2.*pi*(k*x1+l*y1-
     &                   (omega+k*u+l*v)*t))+
     &           k*f*cos(2.*pi*(k*x1+l*y1-
     &                   (omega+k*u+l*v)*t))
	temp=temp0*temp
	uigw=r*temp0*uigw/((2.*pi*omega)**2-f**2)
	vigw=r*temp0*vigw/((2.*pi*omega)**2-f**2)
        return
        end
