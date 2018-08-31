c***************************************************************************c
c                                                                           c
c  Program: SOLVE_traject_3d_v12                                            c
c  reads UKMO,NMC or ECMWF assimilated isentropic level data and computes   c
c  isentropic trajectories using a fourth-order Runge-Kutta time step       c
c                                                                           c
c  Input data:           assimilated u,v,p,pv,q                             c
c  Input parameter file: SOLVE_traject_3d_v13.fil                           c
c  Output data:          trajectory x,y,z                                   c
c                                                                           c
c  Programed by: R. Bradley Pierce 24-aug-1993                              c
c                MS401B TSB                                                 c
c                NASA LARC                                                  c
c                                                                           c
c  Version 2 updates:                                                       c
c                                                                           c
c  Programed by T. Duncan Fairlie 3-nov-1993                                c
c               MS401B TSB                                                  c
c               STC                                                         c
c                                                                           c
c  replaced 24 (hours) by dtflow in expressions for ystep and nstep         c
c  removed nc, nr and x, y from output file header separate subroutines     c
c  for writing and reading header information changed unit number for       c
c  restart file from 10 to 13 to avoid confusion with flow field files      c
c  removed variables day1 and dayn (not used anywhere)                      c
c  removed variable DAY from main program (incremented but not used)        c
c  introduced subroutine calendar to provide current calendar time          c
c  added integer*4 variables istime, ictime of the form "yymmddhh"          c
c  to parameter block comm_ashoe_3d_v08. istime is read in as input         c
c  used dtout as criterion for writing out output file. wtime is the        c
c  accumulated time between writes. subroutine write_output created.        c
c  nrday replaced with irtime - restart time in "yymmddhh" format           c
c                                                                           c
c  additional updates: changed stop criteria for streamline initialization  c
c                      to avoid overshooting initial longitude              c
c                      Pierce 12/7/93                                       c
c                                                                           c
c                      corrected overwrite of xp,yp in interp which caused  c
c                      <10km 'pearl necklace' problems as parcels passed    c
c                      1dx from 0 degrees (1/24/93)                         c
c                                                                           c 
c ***  fix wtime=-12. initially so that second dump is 36 hours into run
c      with 24 hours therafter
c
c  Version 4 updates:
c
c  test: for back trajectory with 12Z winds initialising at 00Z -
c        set ictime = 00Z time
c        set etime = 12 and wtime = 12 initially
c        call initialisation when time becomes 0.0
c        initialisation subroutine introduced
c
c  Version 6 updates:
c        additional option of initialising parcels inside the ER2
c        range ring: changes to comm_ashoe_v06 and header information
c
c  Version 7 updates:
c        inclusion of daylit and night-time fractional exposure
c        for each parcel: frday(nmax),frnght(nmax)  between outputs.
c        julday and gmt, and solar zenith angle calculations added.
c 						Duncan 4/10/97
c  Version 8 updates:
c        reads ecmwf data
c        3 dimensional
c        new calendar subroutine 		VLH 1/28/98
c
c  Version 9 updates:
c        reads UKMO netCDF isentropic data	VLH 9/22/98
c        if streamline initialisation then reads UKMO nondivergent winds
c        needs comm_ukmo_3d_v09. to declare xmsf arrays
c
c  Version 10 updates:
c        changed er2leg to flight_path          VLH 10/14/98
c        capable of initializing an ER2 flight track or DIAL curtain
c        requires v10 of .fil to read in DIAL parameters
c        v09 comm file contains v10 DIAL parameters
c
c  Version 11 updates:
c        introduce time-dependent initialisation of DIAL data
c        requires v11 of .fil to read in DIAL parameters
c        v11 comm file contains v11 DIAL parameters
c                                              Duncan 6/28/99 
c  Version 12 updates:
c        introduce velocity potential and stream function
c        2 marker fields read, xmrkpv and xmrksf    
c                                              Duncan 10/13/99
c                                                                           c
c  'etime' is the elapsed time of the model run                             c
c  Version 13 updates:
c        introduced t0n and agen variables
c          (initial time and age) for DIAL applications
c                                              Duncan 11/05/99
c  Version 15 updates:
c        introduced minimum temperature variable xmint 
c                                              Duncan 7/18/2000
c
c  Version 16 updates:
c        introduce 3 .nc4 marker fields
c        xmrkpvl, xmrksfl, xmrksfh
c                                              VLH 8/24/2001
c
c  Retain discrete nature of the vortex marker field.  Do not interpolate
c  in time or space.  In time, change value at 12Z.  In space, assign the
c  majority of vertex values.  In case of a tie set to zero.
c                                              VLH 5/1/2002
c
c  Version 17 updates:
c        include option to initialize annulus around dc8 flight track
c    altitude
c                                              VLH 9/10/2002
c
c  Version 18 updates:
c        read only 1 marker field (xmrksf) that is equivalent to xmrksfl and 
c    xmrksfh. polar vortices are marked 1 and anticyclones are negative integers
c    remove streamfunction and velocity potential fields
c                                              VLH 12/4/2002
c
c  Version 20 updates:
c        adapted directly from Version 18.  Run with SLIMCAT isentropic winds.
c        set dth/dt=0
c
c  Version WACCM - read WACCM data generated by files from Hanli Liu
c  /aura7/harvey/WACCM_data/Datfiles/Datfiles_Liu/WA3548T08CO_2x.cam2.h2.YYYY-MM-DD-MetO.nc
c                                              VLH 8/2004
c***************************************************************************c
      program WACCM_traject_3d_v20_macd88
c
      include "netcdf.inc"
c
      include 'comm_ukmo_3d_v20.'
c
      REAL UVELOLD(NRmax,NCmax,nthwmx) 
      REAL VVELOLD(NRmax,NCmax,nthwmx) 
      REAL QVELOLD(NRmax,NCmax,nthwmx) 
      REAL POLD(NRmax,NCmax,nthwmx) 
      REAL PVOLD(NRmax,NCmax,nthwmx) 
      REAL XMSFOLD(NRmax,NCmax,nthwmx)
      REAL QDFOLD(NRmax,NCmax,nthwmx) 
      REAL XMRKOLD(NRmax,NCmax,nthwmx)
      REAL XNOOLD(NRmax,NCmax,nthwmx)
      REAL XCOOLD(NRmax,NCmax,nthwmx)
      REAL XEOLD(NRmax,NCmax,nthwmx)
C
      REAL UVELNEW(NRmax,NCmax,nthwmx) 
      REAL VVELNEW(NRmax,NCmax,nthwmx)
      REAL QVELNEW(NRmax,NCmax,nthwmx)
      REAL PNEW(NRmax,NCmax,nthwmx) 
      REAL PVNEW(NRmax,NCmax,nthwmx)
      REAL XMSFNEW(NRmax,NCmax,nthwmx)
      REAL QDFNEW(NRmax,NCmax,nthwmx)
      REAL XMRKNEW(NRmax,NCmax,nthwmx)
      REAL XNONEW(NRmax,NCmax,nthwmx)
      REAL XCONEW(NRmax,NCmax,nthwmx)
      REAL XENEW(NRmax,NCmax,nthwmx)
c
      Real arr1(ncmax),arr2(nrmax),arr3(nthwmx)
      real arr4(nrmax,ncmax,nthwmx),arr5(nrmax,ncmax,nthwmx)
      real arr6(nrmax,ncmax,nthwmx),arr7(nrmax,ncmax,nthwmx)
      real arr8(nrmax,ncmax,nthwmx),arr9(nrmax,ncmax,nthwmx)
      real arr10(nrmax,ncmax,nthwmx),arr11(nrmax,ncmax,nthwmx)
      real arr12(nrmax,ncmax,nthwmx),arr13(nrmax,ncmax,nthwmx)
c
      logical ex0  
c
      character*120 savfile
      character*120 test
      character name*20
      integer*4 date
c
      data ex0 /.false./
c
c  read input parameters for this experiment
c
      print *, 'Opening WACCM_traject_3d_v20.fil'
      OPEN(5,FILE='WACCM_traject_3d_v20.fil',status='old',READONLY)
      call read_list_header(5)
c
c  write input parameters for this experiment to day file
c
      call write_list_header(6)
c
c  truncate blank characters from the name of the input/output file directory
c
      len=0
      len1=0
      test=ifile(1)
      do n=1,119 
        if(dir(n:n+1).ne.' ')len=len+1
        if(test(n:n+1).ne.' ')len1=len1+1
      enddo
C
      OPEN(12,FILE=OFILE,status='unknown',FORM='UNFORMATTED')
c
c  write input parameters for this experiment to output file
c
      call write_unf_header(12)
c
c  test for restart from existing trajectory data set
c
c     if(restart) then
c       OPEN(13,FILE=dir(1:len)//rFILE,status='old',
c    &       FORM='UNFORMATTED')
c       call read_restart_data(13)
c       close(13)
c     endif
c
      if ( .not. restart) then
        call zero_arrays
      endif
c
c initialize dependent time loop variables
c
      ystep=abs(60.*dtflow/dt)
      idt=dt
      idtflw=dtflow
      nstep=abs(60*idtflw/idt)
      print *,'ystep= ',ystep
      print *,'nstep= ',nstep
c
c initialize constants
c
      dt=dt*60.
      PI2 = 6.2831853071796
      print *,'dt=',dt, 'seconds'
      RAD=6.37E6
      COUNT=0
      ITFLAG=0 
C 
c  next two lines for back trajectory with 0Z winds & output at 0Z
C      if (.not. restart) etime=0.0    
C      wtime=0.0
      if (.not. restart) etime=0.0 
c
c WACCM valid at 0 Z
c
      wtime=12.0
      wtime=0.0
c     wtime=-12.0  ! fix so that second dump is 36 hours into run and 24 after
c
c  read flow field data from unit 10
c
      print *,dir(1:len)
      print *,test(1:len1)
      if (.not. restart) then
        inquire(file=dir(1:len)//test(1:len1),exist=ex0)
c
c open netCDF dataset w/o write privileges
        if (ex0) then
          ncid = ncopn(dir(1:len)//test(1:len1),
     &                 NCNOWRIT, iret)
          print *,'opening ',dir(1:len)//test(1:len1)
        endif
        if (.not. ex0) then
          print *,' error in locating flow file: ex0 =',ex0
          stop
        endif 
c
c get dimensions
        call ncdinq(ncid, 1, name, nr,  iret)
        call ncdinq(ncid, 2, name, nc,  iret)
        call ncdinq(ncid, 3, name, nth, iret)
        write(6,*)'netCDF dimensions ',nr,nc,nth
        nrw=nr
        ncw=nc
        nthw=nth
        ifirst=0
        call rdwind(ncid,arr1,arr2,arr3,arr4,arr5,
     1      arr6,arr7,arr8,arr9,arr10,arr11,arr12,arr13,
     2      nrw,ncw,nthw,ifirst)
      endif
      if (restart) then
        inquire(file=dir(1:len)//test(1:len1),exist=ex0)
        if (ex0) then
          ncid = ncopn(dir(1:len)//test(1:len1),
     &                 NCNOWRIT, iret)
        endif
        if (.not. ex0) then
          print *,' error in locating flow file: ex0 =',ex0
          stop
        endif 
c
c get dimensions
        call ncdinq(ncid, 1, name, nr,  iret)
        call ncdinq(ncid, 2, name, nc,  iret)
        call ncdinq(ncid, 3, name, nth, iret)
        write(6,*)'netCDF dimensions ',nr,nc,nth
        nrw=nr
        ncw=nc
        nthw=nth
        ifirst=0
        call rdwind(ncid,arr1,arr2,arr3,arr4,arr5,
     1      arr6,arr7,arr8,arr9,arr10,arr11,arr12,arr13,
     2      nrw,ncw,nthw,ifirst)

      endif
c
      wmax=0.
      xlatmax=0
      DO 12 L=1,nthw
      DO 12 I=1,NC
        DO 12 J=1,NR
           ugrd(j,i,l)=usim(j,i,l)
           vgrd(j,i,l)=vsim(j,i,l)
           xmsfgrd(j,i,l)=xmsfsim(j,i,l)
           pgrd(j,i,l)=psim(j,i,l)
           pvgrd(j,i,l)=pvsim(j,i,l)
           qgrd(j,i,l)=qsim(j,i,l)
           qdfgrd(j,i,l)=qdfsim(j,i,l)
           xmrkgrd(j,i,l)=xmrksim(j,i,l)
           xnogrd(j,i,l)=xnosim(j,i,l)
           xcogrd(j,i,l)=xcosim(j,i,l)
           xegrd(j,i,l)=xesim(j,i,l)
12    CONTINUE
      wmax=sqrt(wmax)
 
c  for a restart run, no new trajectory initialisation is needed
      if (restart) goto 25
 
c  starting with clean sheets: initialise trajectories;
c  compute initial gmt and julian day             Duncan 4/10/97
c  when etime becomes 0.0
c
      if ( abs(etime) .lt. 0.1 ) then
c  comment out when initializing at 12Z with 0Z winds
        call initial
        call jul_gmt (istime,julday,gmt)
        if (dial) then
c output initial characteristics of DIAL parcels 
          if (dt .gt. 0.0) then           ! forward trajectories 
            do id = 1, ncount
              idial = id  
              call init_dial(idial)
              write(6,340) idial, ntraj 
            end do
          endif
          if (dt .lt. 0.0) then           ! back trajectories 
            do id = ncount,1,-1
              idial = id 
              call init_dial(idial)
              write(6,340) idial, ntraj 
            end do
          endif 
        endif 
c initialize DC-8 annulus
        if (dc8) then
          if (dt .gt. 0.0) then           ! forward trajectories
            do id = 1, ncount
              idial = id
              call init_dc8(idial)
              write(6,340) idial, ntraj
            end do
          endif
          if (dt .lt. 0.0) then           ! back trajectories
            do id = ncount,1,-1
              idial = id
              call init_dc8(idial)
              write(6,340) idial, ntraj
            end do
          endif
        endif

      endif
  340 format (' # ',I3,' start time                  ',
     1         '                    ntraj =',I6)
c
25    continue
c
C  WRITE OUTPUT
C
      if (ntraj .lt. 1) then
c adjust etime up for call to calendar         Duncan 5/12/2000 
        rtime = etime + 0.0001
        call calendar(istime,rtime,ictime)
        print *,' No trajectories found at output time',ictime 
      else
c  The initial DIAL data includes dynamic characteristics  Dunc 7/21/99
c  The initial DC-8 data includes dynamic characteristics  VLH 9/10/2002
        if (.not. dial .and. .not. dc8) call getdiag
c adjust etime up for call to calendar         Duncan 5/12/2000 
        rtime = etime + 0.0001
        call calendar(istime,rtime,ictime)
        write(6,*)'writing output at time',ictime,' ntraj =',ntraj
        call write_output(12)
      endif
c
c re-zero dial or DC-8 parcels 
      if (dial .or. dc8) then
        ntraj = 0
        if (dt .gt. 0.0) idial = 1
        if (dt .lt. 0.0) idial = ncount 
      endif 
c
c  looping over flow field files, compute trajectories
c
      DO 999 NFILE=1,NFILES
        test=ifile(nfile)
c  Error corrected: len1 reset                            Duncan 12/04/99 
        len1=0
        do n=1,119
          if(test(n:n+1).ne.' ')len1=len1+1
        enddo
C
        inquire(file=dir(1:len)//test(1:len1),exist=ex0)
c
c open netCDF dataset w/o write privileges
        if (ex0) then
          ncid = ncopn(dir(1:len)//test(1:len1),
     &                 NCNOWRIT, iret)
          print *,'opening ',dir(1:len)//test(1:len1)
        endif
c
c get dimensions
        call ncdinq(ncid, 1, name, nr,  iret)
        call ncdinq(ncid, 2, name, nc,  iret)
        call ncdinq(ncid, 3, name, nth, iret)
        write(6,*)'netCDF dimensions ',nr,nc,nth
        nrw=nr
        ncw=nc
        nthw=nth
        ifirst=0 
        call rdwind(ncid,arr1,arr2,arr3,arr4,arr5,
     1      arr6,arr7,arr8,arr9,arr10,arr11,arr12,arr13,
     2      nrw,ncw,nthw,ifirst)
        DO 10 L = 1,nthw
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
              XMRKOLD(J,I,L)=XMRKNEW(J,I,L)
              XNOOLD(J,I,L)=XNONEW(J,I,L)
              XCOOLD(J,I,L)=XCONEW(J,I,L)
              XEOLD(J,I,L)=XENEW(J,I,L)
            ENDIF
            UVELNEW(J,I,L)=usim(J,I,L) 
            VVELNEW(J,I,L)=vsim(J,I,L) 
            QVELNEW(J,I,L)=qsim(J,I,L) 
            XMSFNEW(J,I,L)=xmsfsim(J,I,L)
            PNEW(J,I,L)=psim(J,I,L) 
            PVNEW(J,I,L)=pvsim(J,I,L) 
            QDFNEW(J,I,L)=qdfsim(J,I,L)
            XMRKNEW(J,I,L)=xmrksim(J,I,L)
            XNONEW(J,I,L)=xnosim(J,I,L)
            XCONEW(J,I,L)=xcosim(J,I,L)
            XENEW(J,I,L)=xesim(J,I,L)
10      CONTINUE
C
        IF(ITFLAG.EQ.0) GO TO 998
C
C  COMPUTE TRAJECTORIES
C
        DO 30 ISTEP=1,NSTEP
c
          XSTEP=ISTEP-1      
          TSCALE=(YSTEP-XSTEP)/YSTEP
          wmax=0.
          DO 20 L=1,nthw
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
              XMRKGRD(J,I,L) = XMRKNEW(J,I,L)
c ******************************************************
              XNOGRD(J,I,L) = XNONEW(J,I,L)+
     &          TSCALE*(XNOOLD(J,I,L)-XNONEW(J,I,L))
              XCOGRD(J,I,L) = XCONEW(J,I,L)+
     &          TSCALE*(XCOOLD(J,I,L)-XCONEW(J,I,L))
              XEGRD(J,I,L) = XENEW(J,I,L)+
     &          TSCALE*(XEOLD(J,I,L)-XENEW(J,I,L))
20        CONTINUE
          wmax=sqrt(wmax)

c  introduce any new DIAL or DC-8 profiles at this timestep   Duncan 6/28/99 
c  Use "date" and "dial_date" and time difference to make the test 
          if (dial .or. dc8) then
c adjust etime up for call to calendar         Duncan 5/12/2000 
            rtime = etime + 0.0001
            call calendar(istime,rtime,ictime)
            dtb2 = 0.5*dt/3600.      ! half timestep in hours 
            date = ictime/100 
            itdial_date = dial_date 
            if (dt .lt. 0.0) then    ! back trajectory condition 
   21         continue 
              if (idial.ge.1) then
                tdial = tsave(idial) 
c  adjust for midnight overlap                 Duncan 4/10/2000
                if (tdial .ge. 24.+dtb2) then
                  tdial = tdial-24.
                  itdial_date = dial_date+1
                endif
                tdiff = abs(abs(tdial)-abs(gmt))
c               write(6,359) ictime, date, itdial_date 
                write(6,360) idial, tdiff, gmt, tdial, ntraj 
                if ((date .eq. itdial_date)
     1             .and. (tdiff .lt. abs(dtb2))) then
                  if (dial) call init_dial(idial)
                  if (dc8) call init_dc8(idial)
                  idial=idial-1
                  goto 21
                endif
              endif
            endif 
            if (dt .ge. 0.0) then    ! forward trajectory condition 
   22         continue 
              if (idial.le.ncount) then
                tdial = tsave(idial) 
c  adjust for midnight overlap                 Duncan 4/10/2000
                if (tdial .ge. 24.-dtb2) then
                  tdial = tdial-24.
                  itdial_date = dial_date+1
                endif
                tdiff = abs(abs(tdial)-abs(gmt))
c               write(6,359) ictime, date, itdial_date 
                write(6,360) idial, tdiff, gmt, tdial, ntraj 
                if ((date .eq. itdial_date)
     1             .and. (tdiff .lt. abs(dtb2))) then
                  if (dial) call init_dial(idial)
                  if (dc8) call init_dc8(idial)
                  idial=idial+1
                  goto 22
                endif 
              endif
            endif
          endif                      ! end of (dial / dc8) condition 
  359 format (' ictime = ',I10,' date = ',I10,' dial_date = ',I10)
  360 format (' # ',I3,' tdiff ',F10.4,' model gmt ',
     1         F10.4,' dial time ',F10.4,' ntraj =',I6)
  361 format (' idial',I4,' istime = ',I10,' etime = ',F10.4,
     1        ' ictime = ',I10)
C
c  DIAL initialisation complete for this timestep     Duncan 6/28/99 
c
          CALL TRAJECT
c
c compute solar zenith angle for each parcel and update 
c fraction of time spent in daylight and nighttime          Duncan 4/10/97
          call solar_fraction
c
c Move call to getdiag out into the timestep loop     Duncan 7/18/2000
          call getdiag
c
          etime=etime+dt/3600.
          wtime=wtime+dt/3600.
c 
c Use the following conditions when the parcel initialisation
c is staggered with respect to the start of the simulation. Duncan 7/1/99 
c e.g. when initializing at 12Z with 0Z winds ......
cDD         if (abs(etime) .gt. 11.9 .AND. abs(etime) .lt. 12.1) then
cDD            call initial
cDD            call jul_gmt (ictime,julday,gmt)
cDD         endif
c
c update gmt and julday after each timestep              Duncan 4/10/97
          gmt = gmt + dt/3600.
          print *,gmt,julday,dt/(3600.*24.)
          if (gmt .lt. 0.0) then
            julday = julday - 1
            gmt = 24. + gmt
          endif
          if (gmt .gt. 24.) then
            julday = julday + 1
            gmt = gmt - 24.
          endif
c         julday=julday+dt/(3600.*24.)
          if (julday .gt. 365) julday=julday-365
          if (julday .lt. 0) julday=365-julday
c         if (julday .gt. 365) then
c           julday = julday - 365
c         endif
c         if (julday .lt. 0) then
c           julday = 365 - julday
c         endif
          write(6,*)'completed ',etime,' hours',
     1              ' jday =',julday,' gmt =',gmt
c
c  test for time to write to output file
c
          if ( (abs(wtime).lt.0.1) .or. 
     +         (abs (abs(wtime)-abs(dtout)) .lt. 0.1) ) then
            if (ntraj .lt. 1) then
c Round etime to whole number of hours when output condition
c satisfied. This should regularly correct any truncation error   
c in etime caused by successive additions of dt.  Duncan 5/12/2000
              etime = nint(etime)*1.0
c adjust etime up for call to calendar         Duncan 5/12/2000 
              rtime = etime + 0.0001
              call calendar(istime,rtime,ictime)
              write(6,*)'current time is',ictime
              print *,' No trajectories found at output time'
            else
c
C add additional calls to spacefill every 10 days  Duncan 7/8/2000
C              if (space) then
C                if ((NFILE .GT. 1).and.(MOD(NFILE,10).EQ.1)) then
C                  ntraj0 = ntraj 
C                  call spacefill 
c
c initialise daylit and nighttime fractions        Duncan 4/10/97
C                  do itraj = ntraj0+1, ntraj
C                    frday(itraj) = 0.0
C                    frnght(itraj) = 0.0
c Adding t0n and agen variables                    Duncan 11/05/99
C                    t0n(itraj) = 0.0
C                    agen(itraj) = 0.0
c Adding xmint variable                            Duncan 7/18/2000
C                    xmint(itraj) = 99999.
C                  end do
C                endif 
C              endif 
c
c Move call to getdiag out into the timestep loop  Duncan 7/18/2000
c              call getdiag
c
c round etime to whole number of hours when output condition
c satisfied. This should regularly correct any truncation error   
c in etime caused by successive additions of dt.  Duncan 5/12/2000
              etime = nint(etime)*1.0
c adjust etime up for call to calendar         Duncan 5/12/2000 
              rtime = etime + 0.0001
              call calendar(istime,rtime,ictime)
              write(6,*)'writing output, ntraj =',ntraj
              write(6,*)'istime ',istime,' etime ',etime,
     1                  ' current time ',ictime
              call write_output(12)
c refresh daylit and nighttime fractions              Duncan 4/10/97
              do itraj = 1, ntraj
                frday(itraj) = 0.0
                frnght(itraj) = 0.0
              end do
c refresh minimum temperature                         Duncan 7/18/2000
              do itraj = 1, ntraj
                xmint(itraj) = 99999.
              end do 
            endif
            wtime=0.0
          endif
c
30      CONTINUE
        close(10)
998     ITFLAG=1
999   CONTINUE		
      STOP
      END 

      subroutine initial
      include 'comm_ukmo_3d_v20.'
c
c     initialise trajectories
c
      ntraj=0
c
c  initialize streamlines
c
c new streamline initialisation based on stream function  Duncan 10/13/99
      if (stream) then 
         ciso = strm0 * 1.0e-6 
         ds = ds1
         do j = 1, nstrm
           call isopleth2(ciso,ds)
           write(6,*)'line',j,'sf =',ciso,' ntraj=',ntraj
           ciso = ciso + dstrm * 1.0e-6 
         end do
      endif 
c 
c old streamline initialisation based on rotational winds  Duncan 10/13/99
c      if (stream) then 
c        n=0
c loop over parcel theta levels                           Duncan 10/14/97
c        do l = 1, nthp
c          dxlat1=dxlat
c          xlat1=xlat0
c          do 15 j=1,nstrm
c             xlon1=0.
c            if (n.lt.nmax)then
c              n=n+1
c              call streamline(n,xlat1,xlon1,theta)
c              write(6,*)j,n,xlat1
c              xlat1=xlat1+dxlat1
c            else 
c              goto 17
c            endif
c15        continue
c        end do
c17      if(n.gt.nmax)stop 'n gt nmax'
c        ntraj=n
c      endif
c
c initialize Potential Vorticity Isopleths
c 
      if(pviso) then 
c
c  smooth pv at poles
c
        pvnp=0.
        pvsp=0.
        do l=1,nth
        do i=1,nc 
          pvsp=pvsp+pvsim(2,i,l)/nc
          pvnp=pvnp+pvsim(nr-1,i,l)/nc
        enddo
        do i=1,nc 
          pvsim(1,i,l)=pvsp
          pvsim(nr,i,l)=pvnp
        enddo
        enddo
        ciso=pv0
        ds=ds2
        do n=1,npviso
          call isopleth2(ciso,ds)
          ciso=ciso+dpv
        enddo
      endif
c
c initialize Temperature Isopleths
c 
      if(tiso) then 
        ciso=temp0
        ds=ds3
        do n=1,ntiso
          call isopleth2(ciso,ds)
          write(6,*)'line',n,'temp=',ciso,' ntraj=',ntraj
          ciso=ciso+dtemp
        enddo
      endif
c
c initialize along ER-2 flight path or DIAL curtain
c 
cDD      if(er2.or.dial) call flight_path(theta)
c  separate er2 and dial initialisation             Duncan 6/28/99 
      if(er2) call flight_path(theta)
c
c  read in dial data for the start day              Duncan 6/28/99  
      if (dial) call read_dial  
c
c  read in dc-8 annulus for the start day           VLH 9/10/2002
      if (dc8) call read_dc8_annulus
c
c initialize space filling
c 
      if(space) call spacefill
c
c initialize filling of ER2 range ring
c 
      if(range_fill) call er2_range_fill
c
c initialise daylit and nighttime fractions              Duncan 4/10/97
      do itraj = 1, ntraj
        frday(itraj) = 0.0
        frnght(itraj) = 0.0
c Adding t0n and agen variables                          Duncan 11/05/99 
        t0n(itraj) = 0.0 
        agen(itraj) = 0.0
c Adding xmint variable                                  Duncan 7/18/2000 
        xmint(itraj) = 99999.
      end do
c   
      print *,'initialisation completed'
      return
      end
      subroutine jul_gmt (istime,julday,gmt)
c
c*****************************************************************c
c  From the start time, istime, compute the Julian day and GMT    c
c  istime is in "yymmddhh" format.                                c
c                                             Duncan 4/10/97      c
c*****************************************************************c
c
      DIMENSION MNO(12)        
      integer*4 istime
      integer*4 yy1,mm1,dd1,hh1
      data (MNO(I),I=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/    
C                                                                       
      yy1=istime/1000000
      mm1=istime/10000-yy1*100
      dd1=istime/100-yy1*10000-mm1*100
      hh1=istime-yy1*1000000-mm1*10000-dd1*100
c
      print *,'Jul_gmt ',yy1,mm1,dd1,hh1
      julday = dd1
      if (mm1 .gt. 1) then
        do m = 1, mm1-1
          julday = julday + mno(m)
        end do
      endif
      gmt = 1.0 * hh1
      print *,'Jul_gmt ',julday,gmt
      return
      end
      subroutine solar_fraction
c*****************************************************************c
c  Compute solar zenith angle for each parcel and update          c
c  daylit and nighttime fractions                                 c
c  frac_time is the fraction of an output time interval in each   c
c  timestep. julday (Julian day) and gmt (Greenwich Mean Time)    c
c  are continually updated and included in 'comm_ashoe_3d_v08.    c
c                                     Duncan 4/10/97              c
c*****************************************************************c
      include 'comm_ukmo_3d_v20.'
      if (ntraj .ge. 1) then
        frac_time = abs(dt) / (3600.*dtout)
        doy = 1. * julday
        do itraj = 1, ntraj
          rlat = yn(itraj)
          rlon = xn(itraj)
          call zenith_angle(doy,gmt,rlat,rlon,chi)
          if (chi .le. 90) then
            frday(itraj) = frday(itraj) + frac_time
          endif
          if (chi .gt. 90) then
            frnght(itraj) = frnght(itraj) + frac_time
          endif
        end do
      endif
      return
      end
      subroutine zenith_angle(doy,gmt,rlat,rlon,chi)
c*****************************************************************c
c  Compute solar zenith angle for each parcel                     c
c                                                                 c
c INPUT:                                                          c
c  doy   : julian day (INPUT)                                     c
c  gmt   : greenwich mean time (INPUT)                            c
c  tlat  : true latitude  (INPUT)                                 c
c  tlon  : true longitude (INPUT)                                 c
c OUTPUT:                                                         c
c  chi   : zenith angle (degrees) at position (tlat,tlon)         c
c*****************************************************************c
c
c  zenith angle computation
      pi=3.14159265
      dtor=pi/180.
      earinc=23.5
c
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
      subroutine write_output(iout)
c
c*****************************************************************c
c                                                                 c
c  Write to output file, unit iout                                c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      print *,'write_output ',istime,ictime,etime,ntraj
      WRITE(iout)istime,ictime,etime,ntraj
      write(iout)'xtraj'
      WRITE(iout)(xn(j),j=1,ntraj)
      write(iout)'ytraj'
      WRITE(iout)(yn(j),j=1,ntraj)
      write(iout)'ztraj'
      WRITE(iout)(zn(j),j=1,ntraj)
      write(iout)'pvtrj'
      WRITE(iout)(pvn(j),j=1,ntraj)
      write(iout)'ptraj'
      WRITE(iout)(pn(j),j=1,ntraj)
      write(iout)'fdtraj'
      WRITE(iout)(frday(j),j=1,ntraj)
      write(iout)'fntraj'
      WRITE(iout)(frnght(j),j=1,ntraj)
c add additional characteristics to output       Duncan 7/14/99
      write(iout)'qtraj'
      WRITE(iout)(qn(j),j=1,ntraj)
      write(iout)'qdftraj'
      WRITE(iout)(qdfn(j),j=1,ntraj)
      write(iout)'xmsftraj'
      WRITE(iout)(xmsfn(j),j=1,ntraj)
      write(iout)'xmrksf_traj'
      WRITE(iout)(xmrkn(j),j=1,ntraj)
c Adding t0n and agen variables                  Duncan 11/05/99
      write(iout)'t0_traj'
      WRITE(iout)(t0n(j),j=1,ntraj)
      write(iout)'age_traj'
      WRITE(iout)(agen(j),j=1,ntraj)
c Adding xmint variable                          Duncan 7/18/2000
      write(iout)'mint_traj'
      WRITE(iout)(xmint(j),j=1,ntraj)
c Adding NO variable                          VLH 8/24/2010
      write(iout)'no_traj'
      WRITE(iout)(xnon(j),j=1,ntraj)
c Adding CO variable                          VLH 8/24/2010
      write(iout)'co_traj'
      WRITE(iout)(xcon(j),j=1,ntraj)
c Adding electrons variable                   VLH 8/24/2010
      write(iout)'e_traj'
      WRITE(iout)(xen(j),j=1,ntraj)

      return
      end

      subroutine read_list_header(iunit)
c
c*****************************************************************c
c                                                                 c
c  Read header information, list directed, from unit iunit        c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      read(iunit,*)charexp
      read(iunit,*)nthp
      read(iunit,*)(thp(k),k=1,nthp)
      read(iunit,*)ukmo
      read(iunit,*)nmc
      read(iunit,*)ecmwf
      read(iunit,*)restart
      read(iunit,*)rfile
      read(iunit,*)irtime
      read(iunit,*)dir
      read(iunit,*)nfiles
      do n=1,nfiles
         read(iunit,*)ifile(n)
         print *, n,nfiles
      enddo
      read(iunit,*)istime
      read(iunit,*)dtflow
      read(iunit,*)dt
      read(iunit,*)igw
      read(iunit,*)stream
      read(iunit,*)nstrm
      read(iunit,*)ds1
cDD      read(iunit,*)xlat0                       Duncan 10/13/99
cDD      read(iunit,*)dxlat                       Duncan 10/13/99 
c choose a lowest stream function value and increment
      read(iunit,*)strm0
      read(iunit,*)dstrm
      read(iunit,*)pviso
      read(iunit,*)npviso
      read(iunit,*)ds2
      read(iunit,*)pv0
      read(iunit,*)dpv
      read(iunit,*)tiso
      read(iunit,*)ntiso
      read(iunit,*)ds3
      read(iunit,*)temp0
      read(iunit,*)dtemp
      read(iunit,*)space
      read(iunit,*)dxs
      read(iunit,*)dys
      read(iunit,*)range_fill
      read(iunit,*)stlat
      read(iunit,*)stlon
      read(iunit,*)er2_range
      read(iunit,*)nrings
      read(iunit,*)npts
      read(iunit,*)dial
      read(iunit,*)dird
      read(iunit,*)ndial
      do n=1,ndial
         read(iunit,*)dfile(n)
         print *, n, dfile(n)
      enddo
      read(iunit,*)er2
      read(iunit,*)nleg
      do n=1,nleg
         read(iunit,*)xleg0(n)
         read(iunit,*)yleg0(n)
         read(iunit,*)xleg1(n)
         read(iunit,*)yleg1(n)
         print *, n, nleg
      enddo
      read(iunit,*)dc8
      read(iunit,*)dirdc8
      read(iunit,*)ndc8
      do n=1,ndc8
         read(iunit,*)dc8file(n)
         print *, n, dc8file(n)
      enddo
      read(iunit,*)dtout
      read(iunit,*)ofile
      print *, ofile
      return
      end
      subroutine write_list_header(iunit)
c
c*****************************************************************c
c                                                                 c
c  Write header information, list directed, to unit iunit         c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      write(iunit,*)charexp
      write(iunit,*)nthp
      write(iunit,*)(thp(k),k=1,nthp) 
      write(iunit,*)ukmo
      write(iunit,*)nmc
      write(iunit,*)ecmwf
      write(iunit,*)restart
      write(iunit,*)rfile
      write(iunit,*)irtime
      write(iunit,*)dir
      write(iunit,*)nfiles
      do n=1,nfiles
         write(iunit,*)ifile(n)
      enddo
      write(iunit,*)istime
      write(iunit,*)dtflow
      write(iunit,*)dt
      write(iunit,*)igw
      write(iunit,*)stream
      write(iunit,*)nstrm
      write(iunit,*)ds1
cDD      write(iunit,*)xlat0                       Duncan 10/13/99
cDD      write(iunit,*)dxlat                       Duncan 10/13/99 
c choose a lowest stream function value and increment
      write(iunit,*)strm0                           
      write(iunit,*)dstrm                           
      write(iunit,*)pviso
      write(iunit,*)npviso
      write(iunit,*)ds2
      write(iunit,*)pv0
      write(iunit,*)dpv
      write(iunit,*)tiso
      write(iunit,*)ntiso
      write(iunit,*)ds3
      write(iunit,*)temp0
      write(iunit,*)dtemp
      write(iunit,*)space
      write(iunit,*)dxs
      write(iunit,*)dys
      write(iunit,*)range_fill
      write(iunit,*)stlat
      write(iunit,*)stlon
      write(iunit,*)er2_range
      write(iunit,*)nrings
      write(iunit,*)npts
      write(iunit,*)dial
      write(iunit,*)dird
      write(iunit,*)ndial
      do n=1,ndial
         write(iunit,*)dfile(n)
      enddo
      write(iunit,*)er2
      write(iunit,*)nleg
      do n=1,nleg
         write(iunit,*)xleg0(n)
         write(iunit,*)yleg0(n)
         write(iunit,*)xleg1(n)
         write(iunit,*)yleg1(n)
      enddo
      write(iunit,*)dc8
      write(iunit,*)dirdc8
      write(iunit,*)ndc8
      do n=1,ndc8
         write(iunit,*)dc8file(n)
      enddo
      write(iunit,*)dtout
      write(iunit,*)ofile
      return
      end
      subroutine write_unf_header(iunit)
c
c*****************************************************************c
c                                                                 c
c  Write header information, unformatted, to output file          c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      write(iunit)charexp                         
      write(iunit)nthp
      write(iunit)(thp(k),k=1,nthp)
      write(iunit)ukmo                            
      write(iunit)nmc                             
      write(iunit)ecmwf                           
      write(iunit)restart
      write(iunit)rfile
      write(iunit)irtime
      write(iunit)dir                             
      write(iunit)nfiles                          
      do n=1,nfiles                           
         write(iunit)ifile(n)                        
      enddo                                    
      write(iunit)istime
      write(iunit)dtflow                          
      write(iunit)dt                              
      write(iunit)igw                             
      write(iunit)stream                          
      write(iunit)nstrm                           
      write(iunit)ds1                             
cDD      write(iunit)xlat0                       Duncan 10/13/99
cDD      write(iunit)dxlat                       Duncan 10/13/99 
c choose a lowest stream function value and increment
      write(iunit)strm0                           
      write(iunit)dstrm                           
      write(iunit)pviso                           
      write(iunit)npviso                          
      write(iunit)ds2                             
      write(iunit)pv0                             
      write(iunit)dpv                             
      write(iunit)tiso                            
      write(iunit)ntiso                           
      write(iunit)ds3                             
      write(iunit)temp0                           
      write(iunit)dtemp                           
      write(iunit)space                           
      write(iunit)dxs                             
      write(iunit)dys                             
      write(iunit)range_fill
      write(iunit)stlat
      write(iunit)stlon
      write(iunit)er2_range
      write(iunit)nrings
      write(iunit)npts
      write(iunit)dial
      write(iunit)dird
      write(iunit)ndial
      do n=1,ndial
         write(iunit)dfile(n)
      enddo
      write(iunit)er2                             
      write(iunit)nleg                            
      do n=1,nleg                              
        write(iunit)xleg0(n)                        
        write(iunit)yleg0(n)                        
        write(iunit)xleg1(n)                        
        write(iunit)yleg1(n)                        
      enddo                                    
      write(iunit)dc8
      write(iunit)dirdc8
      write(iunit)ndc8
      do n=1,ndc8
         write(iunit)dc8file(n)
      enddo
      write(iunit)dtout                           
      write(iunit)ofile
      return
      end
      subroutine read_restart_data(iunit)
c
c*****************************************************************c
c                                                                 c
c   Read restart data, unformatted, from unit iunit               c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      character*5 blank
c
c  first skip over header information in restart unit
c
      do 10 k = 1, 9
   10   read(iunit)
      read(iunit)nrf
      write (6,*) ' nrf = ',nrf
      do n=1,nrf
        read(iunit)
      enddo
      do 20 k = 1, 23
   20   read(iunit)
      read(iunit)nrl
      write (6,*) ' nrl = ',nrl
      do n=1,nrl
        read(iunit)
        read(iunit)
        read(iunit)
        read(iunit)
      enddo
      read(iunit)dtout
      read(iunit)ofile
c
c skip to restart day
c
   40 continue
      write(6,*)'reading restart data '
      READ(iunit) istime,ictime,time,ntraj
      write (6,*) 'data found for time',ictime
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (xn(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (yn(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (zn(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (pvn(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (pn(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (frday(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (frnght(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (qn(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (qdfn(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (xmsfn(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (xmrkn(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (t0n(j),j=1,ntraj)
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (agen(j),j=1,ntraj)
c Add xmint variable                                 Duncan 7/18/2000
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (xmint(j),j=1,ntraj)
c Add NO variable                                    VLH 8/24/2010
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (xnon(j),j=1,ntraj)
c Add CO variable                                    VLH 8/24/2010
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (xcon(j),j=1,ntraj)
c Add electrons variable                             VLH 8/24/2010
      read(iunit) blank
      write(6,*)blank
      READ(iunit) (xen(j),j=1,ntraj)

      if (ictime .lt. irtime-1) goto 40
      if (ictime .gt. irtime+1) then
        write (6,*) 'error: restart data for time ',irtime
        write (6,*) 'not found in restart file'
        stop
      endif
C
      return
      end
      subroutine rdwind(ncid,arr1,arr2,arr3,arr4,arr5,
     1      arr6,arr7,arr8,arr9,arr10,arr11,arr12,arr13,
     2      nrw,ncw,nthw,ifirst)
c
c*****************************************************************c
c                                                                 c
c   Read flow field data                                          c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
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

      write(6,*)' READING 3D ISENTROPIC DATA ......'

      print *,'ncid ',ncid
      call ncvgt(ncid, 1, 1, ncw, arr1, iret)
      call ncvgt(ncid, 2, 1, nrw, arr2, iret)
      call ncvgt(ncid, 3, 1, nthw, arr3, iret)
      call ncvgt(ncid, 4, istart, icount, arr4, iret)
      call ncvgt(ncid, 5, istart, icount, arr5, iret)
      call ncvgt(ncid, 6, istart, icount, arr6, iret)
      call ncvgt(ncid, 7, istart, icount, arr7, iret)
      call ncvgt(ncid, 8, istart, icount, arr8, iret)
      call ncvgt(ncid, 9, istart, icount, arr9, iret)
      call ncvgt(ncid, 10, istart, icount, arr10, iret)
      call ncvgt(ncid, 11, istart, icount, arr11, iret)
      call ncvgt(ncid, 12, istart, icount, arr12, iret)
      call ncvgt(ncid, 13, istart, icount, arr13, iret)
      call ncclos(ncid, iret)
C
      dlon=arr1(2)-arr1(1)
      dlat=arr2(2)-arr2(1)
      do i=1,nc
        x(i)=arr1(i)
      enddo
      do j=1,nr
        y(j)=arr2(j)
      enddo
      do k=1,nth
        thw(k)=arr3(k)
      enddo
      do i=1,nc
        do j=1,nr
          do l=1,nth
             pvsim(j,i,l)=arr4(j,i,l)
             psim(j,i,l)=arr5(j,i,l)
             usim(j,i,l)=arr6(j,i,l)
             vsim(j,i,l)=arr7(j,i,l)
             qdfsim(j,i,l)=arr8(j,i,l)
             qsim(j,i,l)=arr9(j,i,l)/86400.
             xmsfsim(j,i,l)=arr10(j,i,l)	! GPH
             xnosim(j,i,l)=arr11(j,i,l)		! TTGW
             xcosim(j,i,l)=arr12(j,i,l)		! SF
             xmrksim(j,i,l)=arr13(j,i,l)	! MARK
          enddo
        enddo
      enddo
      return
      end
      subroutine getdiag
c
c*****************************************************************c
c                                                                 c
c   Diagnose PV and P for each air parcel                         c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      real xmsfj1,xmsfjp1,xmsfj2,xmsfjp2,xmsf1,xmsf2

      nthw = nth 
      do 50 n=1,ntraj
        xp=xn(n)
        yp=yn(n)
        zp=zn(n)
        if(xp.lt.x(1))xp=xp+360.
        do 40 l=1,nthw-1
          lp1=l+1
          thl=thw(l)
          thlp1=thw(lp1)
c VLH 5/2/02         if((zp.le.thl).and.(zp.gt.thlp1))then
          if((zp.le.thl).and.(zp.ge.thlp1))then
          zscale=(zp-thl)/(thlp1-thl)
          do 30 i=1,nc
            ip1=i+1
            if(i.eq.nc)ip1=1
            xlon=x(i) 
            xlonp1=x(ip1)
            if(i.eq.nc)xlonp1=360.+x(ip1)
c VLH 5/2/02            if((xp.ge.xlon).and.(xp.lt.xlonp1))then
            if((xp.ge.xlon).and.(xp.le.xlonp1))then
            xscale=(xp-xlon)/(xlonp1-xlon)
            do 20 j=1,nr-1
              jp1=j+1
              xlat=y(j)
              xlatp1=y(jp1)
c
c note: does not interpolate across the poles	VLH 5/8/02
c
              if((yp.ge.xlat).and.(yp.le.xlatp1))then
                yscale=(yp-xlat)/(xlatp1-xlat)
c level l
                pvj1=pvgrd(j,i,l)+xscale*(pvgrd(j,ip1,l)-pvgrd(j,i,l))
                 pj1=pgrd(j,i,l)+xscale*(pgrd(j,ip1,l)-pgrd(j,i,l))
              xmsfj1=xmsfgrd(j,i,l)
     +              +xscale*(xmsfgrd(j,ip1,l)-xmsfgrd(j,i,l))
              xmrkj1=xmrkgrd(j,i,l)
     +              +xscale*(xmrkgrd(j,ip1,l)-xmrkgrd(j,i,l))
                 qj1=qgrd(j,i,l)+xscale*(qgrd(j,ip1,l)-qgrd(j,i,l))
               qdfj1=qdfgrd(j,i,l)
     +              +xscale*(qdfgrd(j,ip1,l)-qdfgrd(j,i,l))
              xnoj1=xnogrd(j,i,l)+xscale*(xnogrd(j,ip1,l)-xnogrd(j,i,l))
              xcoj1=xcogrd(j,i,l)+xscale*(xcogrd(j,ip1,l)-xcogrd(j,i,l))
              xej1=xegrd(j,i,l)+xscale*(xegrd(j,ip1,l)-xegrd(j,i,l))

               pvjp1=pvgrd(jp1,i,l)
     +              +xscale*(pvgrd(jp1,ip1,l)-pvgrd(jp1,i,l))
                pjp1=pgrd(jp1,i,l)
     +              +xscale*(pgrd(jp1,ip1,l)-pgrd(jp1,i,l))
             xmsfjp1=xmsfgrd(jp1,i,l)
     +              +xscale*(xmsfgrd(jp1,ip1,l)-xmsfgrd(jp1,i,l))
             xmrkjp1=xmrkgrd(jp1,i,l)
     +              +xscale*(xmrkgrd(jp1,ip1,l)-xmrkgrd(jp1,i,l))
                qjp1=qgrd(jp1,i,l)
     +              +xscale*(qgrd(jp1,ip1,l)-qgrd(jp1,i,l))
              qdfjp1=qdfgrd(jp1,i,l)
     +              +xscale*(qdfgrd(jp1,ip1,l)-qdfgrd(jp1,i,l))
              xnojp1=xnogrd(jp1,i,l)
     +              +xscale*(xnogrd(jp1,ip1,l)-xnogrd(jp1,i,l))
              xcojp1=xcogrd(jp1,i,l)
     +              +xscale*(xcogrd(jp1,ip1,l)-xcogrd(jp1,i,l))
              xejp1=xegrd(jp1,i,l)
     +              +xscale*(xegrd(jp1,ip1,l)-xegrd(jp1,i,l))

c level lp1
                pvj2=pvgrd(j,i,lp1)
     +              +xscale*(pvgrd(j,ip1,lp1)-pvgrd(j,i,lp1))
                 pj2=pgrd(j,i,lp1)
     +              +xscale*(pgrd(j,ip1,lp1)-pgrd(j,i,lp1))
              xmsfj2=xmsfgrd(j,i,lp1)
     +              +xscale*(xmsfgrd(j,ip1,lp1)-xmsfgrd(j,i,lp1))
              xmrkj2=xmrkgrd(j,i,lp1)
     +              +xscale*(xmrkgrd(j,ip1,lp1)-xmrkgrd(j,i,lp1))
                 qj2=qgrd(j,i,lp1)
     +              +xscale*(qgrd(j,ip1,lp1)-qgrd(j,i,lp1))
               qdfj2=qdfgrd(j,i,lp1)
     +              +xscale*(qdfgrd(j,ip1,lp1)-qdfgrd(j,i,lp1))
               xnoj2=xnogrd(j,i,lp1)
     +              +xscale*(xnogrd(j,ip1,lp1)-xnogrd(j,i,lp1))
               xcoj2=xcogrd(j,i,lp1)
     +              +xscale*(xcogrd(j,ip1,lp1)-xcogrd(j,i,lp1))
               xej2=xegrd(j,i,lp1)
     +              +xscale*(xegrd(j,ip1,lp1)-xegrd(j,i,lp1))

               pvjp2=pvgrd(jp1,i,lp1)+
     +              +xscale*(pvgrd(jp1,ip1,lp1)-pvgrd(jp1,i,lp1))
                pjp2=pgrd(jp1,i,lp1)
     +              +xscale*(pgrd(jp1,ip1,lp1)-pgrd(jp1,i,lp1))
             xmsfjp2=xmsfgrd(jp1,i,lp1)
     +              +xscale*(xmsfgrd(jp1,ip1,lp1)-xmsfgrd(jp1,i,lp1))
             xmrkjp2=xmrkgrd(jp1,i,lp1)
     +              +xscale*(xmrkgrd(jp1,ip1,lp1)-xmrkgrd(jp1,i,lp1))
                qjp2=qgrd(jp1,i,lp1)
     +              +xscale*(qgrd(jp1,ip1,lp1)-qgrd(jp1,i,lp1))
              qdfjp2=qdfgrd(jp1,i,lp1)
     +              +xscale*(qdfgrd(jp1,ip1,lp1)-qdfgrd(jp1,i,lp1))
              xnojp2=xnogrd(jp1,i,lp1)
     +              +xscale*(xnogrd(jp1,ip1,lp1)-xnogrd(jp1,i,lp1))
              xcojp2=xcogrd(jp1,i,lp1)
     +              +xscale*(xcogrd(jp1,ip1,lp1)-xcogrd(jp1,i,lp1))
              xejp2=xegrd(jp1,i,lp1)
     +              +xscale*(xegrd(jp1,ip1,lp1)-xegrd(jp1,i,lp1))

c level l
                 pv1=pvj1+yscale*(pvjp1-pvj1)
                  p1=pj1+yscale*(pjp1-pj1)
               xmsf1=xmsfj1+yscale*(xmsfjp1-xmsfj1)
               xmrk1=xmrkj1+yscale*(xmrkjp1-xmrkj1)
                  q1=qj1+yscale*(qjp1-qj1)
                qdf1=qdfj1+yscale*(qdfjp1-qdfj1)
                  xno1=xnoj1+yscale*(xnojp1-xnoj1)
                  xco1=xcoj1+yscale*(xcojp1-xcoj1)
                  xe1=xej1+yscale*(xejp1-xej1)
c level lp1
                 pv2=pvj2+yscale*(pvjp2-pvj2)
                  p2=pj2+yscale*(pjp2-pj2)
               xmsf2=xmsfj2+yscale*(xmsfjp2-xmsfj2)
               xmrk2=xmrkj2+yscale*(xmrkjp2-xmrkj2)
                  q2=qj2+yscale*(qjp2-qj2)
                qdf2=qdfj2+yscale*(qdfjp2-qdfj2)
                xno2=xnoj2+yscale*(xnojp2-xnoj2)
                xco2=xcoj2+yscale*(xcojp2-xcoj2)
                xe2=xej2+yscale*(xejp2-xej2)
c
                pvn(n)=pv1+zscale*(pv2-pv1)
c linear interpolation in log pressure		VLH 7/23/03
c                pn(n)=p1+zscale*(p2-p1)
                 pp=alog(p1)+zscale*alog(p2/p1)
                 pn(n)=exp(pp)
              xmsfn(n)=xmsf1+zscale*(xmsf2-xmsf1)
              xmrkn(n)=xmrk1+zscale*(xmrk2-xmrk1)
                 qn(n)=q1+zscale*(q2-q1)
               qdfn(n)=qdf1+zscale*(qdf2-qdf1)
                 xnon(n)=xno1+zscale*(xno2-xno1)
                 xcon(n)=xco1+zscale*(xco2-xco1)
                 xen(n)=xe1+zscale*(xe2-xe1)
c
                goto 45 
              endif
   20       continue
            endif
   30     continue
          endif
   40   continue
   45   continue
c update minimum temperature                            Duncan 7/18/2000
        rkap=0.286 
        tn = zn(n)*(pn(n)/1000.)**rkap 
        if (tn .lt. xmint(n)) xmint(n) = tn 
   50 continue
      return
      end  
      subroutine calendar (istime,etime,ictime) 
c*****************************************************************c
c  Compute the current time, ictime, from the start time, istime, c
c  and the elapsed time in hours, etime.   Both ictime and istime c
c  are integer*4 and expressed in the form "yymmddhh"             c
c                                                                 c
c  Simulations less than 2 years are supported                    c
c                                                                 c
c  This version accomodates back trajectories (VLH 1/28/98)       c
c  Corrected for 12Z winds situation          (VLH 7/01/99)       c
c*****************************************************************c

      integer*4 istime,ictime,yy1,mm1,dd1,hh1,yy2,mm2,dd2,hh2
      integer mno(12)
      real etime
      data (mno(i),i=1,12)/31,28,31,30,31,30,31,31,30,31,30,31/

      yy1=istime/1000000
      if (mod(yy1,4) .eq. 0.) mno(2)=29
      yy2=yy1
      mm1=istime/10000-yy1*100
      mm2=mm1
      dd1=istime/100-yy1*10000-mm1*100
c
c --- New day                                   Duncan 5/11/2000
C      dd2=dd1+nint(etime)/24
      dd2=dd1+ifunc(etime)/24
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

c --- New hour                                   Duncan 5/11/2000
C      hh2=mod(nint(etime),24)+hh1
      hh2=mod(ifunc(etime),24)+hh1
      if (etime.gt.0.) then
         if (hh2.le. 0) hh2=hh2+24
         if (hh2.ge.24) then
            hh2=hh2-24
            dd2=dd2+1
            if (dd2.gt.mno(mm2)) then
               dd2 = dd2 - mno(mm2)
               mm2=mm2+1
               if (mm2.gt.12) then
                  mm2=mm2-12
                  yy2=yy2+1
               endif
            endif
         endif
      else if (etime.lt.0.) then
         if (hh2.lt. 0) then
            hh2=hh2+24
            dd2=dd2-1
            if (dd2.lt.1) then
               mm2=mm2-1
               if (mm2.lt.1) mm2=12
               dd2=mno(mm2)
               if (mm2.eq.12) yy2=yy2-1
            endif
         endif
      endif

c --- Current time
      ictime=hh2+dd2*100+mm2*10000+yy2*1000000

      return
      end
      function ifunc(etime)
c Function subroutine that returns the integer immediately
c below the real number etime          Duncan 4/21/2000 
      if (etime .ge. 0.0) then
        ifunc = ifix(etime)
      endif
      if (etime .lt. 0.0) then
        ifunc = ifix(etime) 
        del = amod(etime,1.) 
        if (del .lt. 0.0) ifunc = ifunc-1 
      endif
      ifunc = ifunc
      return
      end 
c****************************************************************
      subroutine traject
c
c*****************************************************************c
c                                                                 c
c  Timestep trajectories using fourth-order Runge-Kutta scheme    c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      real m1,n1,l1
      real m2,n2,l2
      real m3,n3,l3
      real m4,n4,l4
      PI2 = 6.2831853071796
      RAD=6.37E6
      radg=pi2/360.
      pi=.5*pi2
c
      do 100 n=1,ntraj
c
        call interp(u,v,q,uigw,vigw,xn(n),yn(n),zn(n))
c
        m1=dt*(180./pi)*u/(rad*cos(radg*yn(n)))
        n1=dt*(180./pi)*v/rad
        l1=dt*q
        xprm = xn(n) + .5*m1
        yprm = yn(n) + .5*n1
        zprm = zn(n) + .5*l1
c
        call interp(u,v,q,uigw,vigw,xprm,yprm,zprm)
c
        m2=dt*(180./pi)*u/(rad*cos(radg*yprm))
        n2=dt*(180./pi)*v/rad
        l2=dt*q
        xprm = xn(n) + .5*m2
        yprm = yn(n) + .5*n2
        zprm = zn(n) + .5*l2
c
        call interp(u,v,q,uigw,vigw,xprm,yprm,zprm)
c
        m3=dt*(180./pi)*u/(rad*cos(radg*yprm))
        n3=dt*(180./pi)*v/rad
        l3=dt*q
        xprm = xn(n) + m3
        yprm = yn(n) + n3
        zprm = zn(n) + l3
c
        call interp(u,v,q,uigw,vigw,xprm,yprm,zprm)
c
        m4=dt*(180./pi)*u/(rad*cos(radg*yprm))
        n4=dt*(180./pi)*v/rad
        l4=dt*q
        xn(n) = xn(n) + (m1+2.*m2+2.*m3+m4)/6.
        yn(n) = yn(n) + (n1+2.*n2+2.*n3+n4)/6.
        zn(n) = zn(n) + (l1+2.*l2+2.*l3+l4)/6.
c Adding agen variable                                Duncan 11/05/99 
        agen(n) = agen(n) + dt/3600.
c
c  adjust for polar points
c
        itest=0
50      continue
        if (yn(n).lt.-90.) xn(n)= 180.+xn(n)
        if (yn(n).lt.-90.) yn(n)= -180-yn(n)
        if (yn(n).gt.90.)  xn(n)= 180.+xn(n)
        if (yn(n).gt.90.)  yn(n)= 180-yn(n)
        if (xn(n).lt.0. .and. xn(n).gt.-360.) xn(n)= 360.+xn(n)
        if (xn(n).ge.360. .and. xn(n).lt.720.) xn(n)= -360.+xn(n)
        itest=itest+1
c   if(itest.le.3)goto 50
100   continue
      return
      end
      subroutine streamline(n,xlat1,xlon1,theta)
c
c*****************************************************************c
c                                                                 c
c   Compute streamlines                                           c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
c     real lat1,lon1,nlat,nlon
      PI2 = 6.2831853071796
      xlat=xlat1
      xlon=xlon1
      secday=60.*60.*24.
      DTR = ASIN(1.0)/90. 
      RAD=6.37E6
      RADG = PI2 / 360.
      FAC20 = 1.0 / TAN(45*RADG)       
      DAY=0
c
      olat=xlat
      oLON=xlon
      slon=olon
      slat=olat
      xn(n)=olon
      yn(n)=olat
      zn(n)=theta
      clat=5.
      clon=1.
      Nnmax=2.*nmax/nstrm
      time=0.
      iflag=0
      olon=xn(n)
      olat=yn(n)
      do 75 ISTEP=1,Nnmax-1 
         call interp(u,v,q,uigw,vigw,olon,olat,theta)
         if (igw) then
            u=u-uigw
            v=v-vigw
         endif
         angle=atan2(v,u)
         dsx=ds1*cos(angle)
         dsy=ds1*sin(angle)
         dlon=dsx*1.e3*(360./(PI2*rad*cos(yn(n)*dtr)))
c
         dlat=dsy*1.e3*(360./(PI2*rad))
c
c  UPDATE TRAJECTORY
c
         xn(N+1)=olon+dlon
         yn(N+1)=olat+dlat
         zn(N+1)=theta
c
c        write(6,*)xn(n+1),yn(n+1),zn(n+1),u,v
         IF (xn(N+1) .LT. 0.) 
     &      xn(N+1)=360.+xn(N+1)
         IF (yn(N+1) .GT. 90.) THEN 
            yn(N+1)=180.-yn(N+1)
            xn(N+1)=180.+xn(N+1)
         ENDIF
         IF (yn(N+1) .LT. -90.) THEN 
            yn(N+1)=-180.-yn(N+1)
            xn(N+1)=180.+xn(N+1)
         ENDIF
         IF (xn(N+1) .GT. 360.) THEN 
            MULT=xn(N+1)/360.
            xn(N+1)=xn(N+1)-MULT*360.
         ENDIF
         n=n+1
         if(n.ge.nmax)return
c
         olon=xn(n)
         olat=yn(n)
         if((abs(olat-slat).gt.30.).or.
     &      (abs(olon-slon).gt.30.))iflag=1
          
         if (iflag.eq.1)then
           if ((abs(olon-slon).le.abs(2.*dlon)).or.
     &          (istep.eq.nnmax).or.(n.ge.nmax)) goto 95
         endif
75    continue
95    continue
      return
      END
      subroutine isopleth2(ciso,ds)
c
c*****************************************************************c
c                                                                 c
c   Initialise parcels on PV isopleths, isotherms                 c
c   or streamlines on selected isentropes (version 12 update)     c 
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
c      print *,'isopleth2 routine called'
c      print *,' tiso=',tiso,' pviso=',pviso,' stream=',stream 
c      print *,'nc=',nc,' nr=',nr
      nthw = nth 
      DX=DLON
C
c loop over selected isentropic levels 
      do 30 kp = 1, nthp
        theta = thp(kp)
        print *,' theta = ',theta
c find selected theta level in flow field data
      l = 0 
      do kw = 1, nthw
        if (abs(theta - thw(kw)) .lt. 0.01) then
          l = kw
          goto 5
        endif
      end do
    5 continue 
      if ( l .eq. 0) then
        print *,' selected theta ',theta,' not found in flow field'
        stop
      endif
c
cDD       DO 20 I=2,NC
      DO 20 I=1,NC
        IM1=I-1
        if (i .eq. 1) im1 = nc 
        DO 20 J=1,NR
          JM1=J-1
          IF(J.EQ.1)JM1=1
          HY=Y(J)-Y(JM1)
          if(pviso) then
            A1=(PVsim(JM1,IM1,L))
            B1=(PVsim(JM1,I,L))
            C1=(PVsim(J,IM1,L))
            D1=(PVsim(J,I,L))
          endif
          if(tiso) then
            A1=theta*(Psim(JM1,IM1,L)/1000.)**.286
            B1=theta*(Psim(JM1,I,L)/1000.)**.286
            C1=theta*(Psim(J,IM1,L)/1000.)**.286
            D1=theta*(Psim(J,I,L)/1000.)**.286
          endif
          if(stream) then
            A1= xmsfsim(JM1,IM1,L)
            B1= xmsfsim(JM1,I,L)
            C1= xmsfsim(J,IM1,L)
            D1= xmsfsim(J,I,L)
          endif
C
C  LOWER LEFT TRIANGLE EMPTY 
C
          IF((A1.GE.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.GE.CISO))THEN
            A=DX*(C1-CISO)/(C1-D1)
            B=HY*(C1-CISO)/(C1-A1)
            II=I
            JJ=J
C      print *,' code 1, A,B = ',A,B,' J = ',J  
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,1,theta)
            GO TO 10
          ENDIF 
C
C  LOWER LEFT TRIANGLE FULL 
C
          IF((A1.LT.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.LT.CISO))THEN
            A=DX*(C1-CISO)/(C1-D1)
            B=HY*(C1-CISO)/(C1-A1)
            II=I
            JJ=J
C      print *,' code 2, A,B = ',A,B,' J = ',J  
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,2,theta)
            GO TO 10
          ENDIF 
C
C  UPPER LEFT TRIANGLE EMPTY 
C
          IF((A1.LT.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.GE.CISO))THEN
            A=DX*(A1-CISO)/(A1-B1)
            B=HY*(A1-CISO)/(A1-C1)
            II=I
            JJ=J
C      print *,' code 3, A,B = ',A,B,' J = ',J 
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,3,theta)
            GO TO 10
          ENDIF 
C
C  UPPER LEFT TRIANGLE FULL 
C
          IF((A1.GE.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.LT.CISO))THEN
            A=DX*(A1-CISO)/(A1-B1)
            B=HY*(A1-CISO)/(A1-C1)
            II=I
            JJ=J
C      print *,' code 4, A,B = ',A,B,' J = ',J  
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,4,theta)
            GO TO 10
          ENDIF 
C
C  UPPER RIGHT TRIANGLE EMPTY 
C
          IF((A1.GE.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.GE.CISO))THEN
            A=DX*(B1-CISO)/(B1-A1)
            B=HY*(B1-CISO)/(B1-D1)
            II=I
            JJ=J
C      print *,' code 5, A,B = ',A,B,' J = ',J 
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,5,theta)
            GO TO 10
          ENDIF 
C
C  UPPER RIGHT TRIANGLE FULL 
C
          IF((A1.LT.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.LT.CISO))THEN
            A=DX*(B1-CISO)/(B1-A1)
            B=HY*(B1-CISO)/(B1-D1)
            II=I
            JJ=J
C      print *,' code 6, A,B = ',A,B,' J = ',J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,6,theta)
            GO TO 10
          ENDIF 
C
C  LOWER RIGHT TRIANGLE EMPTY 
C
          IF((A1.GE.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.LT.CISO))THEN
            A=DX*(D1-CISO)/(D1-C1)
            B=HY*(D1-CISO)/(D1-B1)
            II=I
            JJ=J
C      print *,' code 7, A,B = ',A,B,' J = ',J 
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,7,theta)
            GO TO 10
          ENDIF 
C
C  LOWER RIGHT TRIANGLE FULL
C
          IF((A1.LT.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.GE.CISO))THEN
            A=DX*(D1-CISO)/(D1-C1)
            B=HY*(D1-CISO)/(D1-B1)
            II=I
            JJ=J
C      print *,' code 8, A,B = ',A,B,' J = ',J 
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,8,theta)
            GO TO 10
          ENDIF 
C
C  RIGHT TRAPEZOID EMPTY
C
          IF((A1.GE.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.LT.CISO))THEN
            A=DX*(A1-CISO)/(A1-B1)
            B=DX*(C1-CISO)/(C1-D1)
            II=I
            JJ=J
C      print *,' code 9, A,B = ',A,B,' J = ',J 
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,9,theta)
            GO TO 10
          ENDIF 
C
C  RIGHT TRAPEZOID FULL
C
          IF((A1.LT.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.GE.CISO))THEN
            A=DX*(A1-CISO)/(A1-B1)
            B=DX*(C1-CISO)/(C1-D1)
            II=I
            JJ=J
C      print *,' code 10, A,B = ',A,B,' J = ',J 
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,10,theta)
            GO TO 10
          ENDIF 
C
C  LOWER TRAPEZOID EMPTY 
C
          IF((A1.GE.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.LT.CISO))THEN
            A=HY*(C1-CISO)/(C1-A1)
            B=HY*(D1-CISO)/(D1-B1)
            II=I
            JJ=J
C      print *,' code 11, A,B = ',A,B,' J = ',J 
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,11,theta)
            GO TO 10
          ENDIF 
C
C  LOWER TRAPEZOID FULL 
C
          IF((A1.LT.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.GE.CISO))THEN
            A=HY*(C1-CISO)/(C1-A1)
            B=HY*(D1-CISO)/(D1-B1)
            II=I
            JJ=J
C      print *,' code 12, A,B = ',A,B,' J = ',J 
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,12,theta)
            GO TO 10
          ENDIF 
10      CONTINUE
20    CONTINUE
30    continue
      return
      end
      subroutine isopleth(ciso,ds)
c
c*****************************************************************c
c                                                                 c
c   Initialises parcels on PV isopleths or isotherms              c
c   or streamlines (version 12 update)                            c 
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      print *,'isopleth routine called'
      print *,' tiso=',tiso,' pviso=',pviso,' stream=',stream 
c      print *,'nc=',nc,' nr=',nr
      DX=DLON
      nthw = nth 
      do 30 l = 1, nthw
      theta =thw(l)
      print *,'thw(l)=',theta
      DO 20 I=2,NC
        IM1=I-1
        DO 20 J=1,NR
          JM1=J-1
          IF(J.EQ.1)JM1=1
          HY=Y(J)-Y(JM1)
          if(pviso) then
            A1=(PVsim(JM1,IM1,L))
            B1=(PVsim(JM1,I,L))
            C1=(PVsim(J,IM1,L))
            D1=(PVsim(J,I,L))
          endif
          if(tiso) then
            A1=theta*(Psim(JM1,IM1,L)/1000.)**.286
            B1=theta*(Psim(JM1,I,L)/1000.)**.286
            C1=theta*(Psim(J,IM1,L)/1000.)**.286
            D1=theta*(Psim(J,I,L)/1000.)**.286
          endif
          if(stream) then
            A1= xmsfsim(JM1,IM1,L)
            B1= xmsfsim(JM1,I,L)
            C1= xmsfsim(J,IM1,L)
            D1= xmsfsim(J,I,L)
          endif
C
C  LOWER LEFT TRIANGLE EMPTY 
C
          IF((A1.GE.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.GE.CISO))THEN
            A=DX*(C1-CISO)/(C1-D1)
            B=HY*(C1-CISO)/(C1-A1)
            II=I
            JJ=J
            CALL FILL(ds,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,1,theta)
            GO TO 10
          ENDIF 
C
C  LOWER LEFT TRIANGLE FULL 
C
          IF((A1.LT.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.LT.CISO))THEN
            A=DX*(C1-CISO)/(C1-D1)
            B=HY*(C1-CISO)/(C1-A1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,2,theta)
            GO TO 10
          ENDIF 
C
C  UPPER LEFT TRIANGLE EMPTY 
C
          IF((A1.LT.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.GE.CISO))THEN
            A=DX*(A1-CISO)/(A1-B1)
            B=HY*(A1-CISO)/(A1-C1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,3,theta)
            GO TO 10
          ENDIF 
C
C  UPPER LEFT TRIANGLE FULL 
C
          IF((A1.GE.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.LT.CISO))THEN
            A=DX*(A1-CISO)/(A1-B1)
            B=HY*(A1-CISO)/(A1-C1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,4,theta)
            GO TO 10
          ENDIF 
C
C  UPPER RIGHT TRIANGLE EMPTY 
C
          IF((A1.GE.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.GE.CISO))THEN
            A=DX*(B1-CISO)/(B1-A1)
            B=HY*(B1-CISO)/(B1-D1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,5,theta)
            GO TO 10
          ENDIF 
C
C  UPPER RIGHT TRIANGLE FULL 
C
          IF((A1.LT.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.LT.CISO))THEN
            A=DX*(B1-CISO)/(B1-A1)
            B=HY*(B1-CISO)/(B1-D1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,6,theta)
            GO TO 10
          ENDIF 
C
C  LOWER RIGHT TRIANGLE EMPTY 
C
          IF((A1.GE.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.LT.CISO))THEN
            A=DX*(D1-CISO)/(D1-C1)
            B=HY*(D1-CISO)/(D1-B1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,7,theta)
            GO TO 10
          ENDIF 
C
C  LOWER RIGHT TRIANGLE FULL
C
          IF((A1.LT.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.GE.CISO))THEN
            A=DX*(D1-CISO)/(D1-C1)
            B=HY*(D1-CISO)/(D1-B1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,8,theta)
            GO TO 10
          ENDIF 
C
C  RIGHT TRAPEZOID EMPTY
C
          IF((A1.GE.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.LT.CISO))THEN
            A=DX*(A1-CISO)/(A1-B1)
            B=DX*(C1-CISO)/(C1-D1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,9,theta)
            GO TO 10
          ENDIF 
C
C  RIGHT TRAPEZOID FULL
C
          IF((A1.LT.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.GE.CISO))THEN
            A=DX*(A1-CISO)/(A1-B1)
            B=DX*(C1-CISO)/(C1-D1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,10,theta)
            GO TO 10
          ENDIF 
C
C  LOWER TRAPEZOID EMPTY 
C
          IF((A1.GE.CISO).AND.(B1.GE.CISO).AND.
     &       (C1.LT.CISO).AND.(D1.LT.CISO))THEN
            A=HY*(C1-CISO)/(C1-A1)
            B=HY*(D1-CISO)/(D1-B1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,11,theta)
            GO TO 10
          ENDIF 
C
C  LOWER TRAPEZOID FULL 
C
          IF((A1.LT.CISO).AND.(B1.LT.CISO).AND.
     &       (C1.GE.CISO).AND.(D1.GE.CISO))THEN
            A=HY*(C1-CISO)/(C1-A1)
            B=HY*(D1-CISO)/(D1-B1)
            II=I
            JJ=J
            CALL FILL(DS,A,B,A1,B1,C1,D1,II,JJ,IM1,JM1,12,theta)
            GO TO 10
          ENDIF 
10      CONTINUE
20    CONTINUE
30    continue
      return
      end
      subroutine spacefill
c
c*****************************************************************c
c                                                                 c
c   Fill space with air parcels                                   c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      n=ntraj
      write(6,*),' ntraj =',ntraj,' entering spacefill'
c     if (n .eq. 0) n = 1
      imax=360./dxs
      jmax=180./dys+1
      write(6,*)imax,jmax,dxs,dys
      x0=0
      do i=1,imax
        y0=-90
        do j=1,jmax
c
          do l=1, nthp
             n=n+1
             xn(n)=x0
             yn(n)=y0
             zn(n)=thp(l)
c
c incrementing n after assigning xn,yn,zn results in 1 extra traj
c 
c            n=n+1
             if(n.ge.nmax)return
          enddo
          y0=y0+dys
        enddo
        x0=x0+dxs
      enddo
      ntraj=n
      write(6,*),' ntraj =',ntraj,' exiting spacefill'
      return
      END
      subroutine er2_range_fill
c-----------------------------------------------------------------
c   Call range_ring to fill space in the range of the ER2
c-----------------------------------------------------------------
c
c INPUTS from common_ashoe_v08:   
c         stlat   = center latitude (in degs.)
c         stlon   = center longitude (in degs.)
c         er2_range  = ER2 range from the central point (in KM)
c         npts    = number of points in each ring
c                   (if zero, defaults to 36)
c         nrings = number of rings
c
      include 'comm_ukmo_3d_v20.'
      parameter (nprmax=36)
      real range,bearing(nprmax),xlatp(nprmax),xlonp(nprmax)
c
      n=ntraj
      write(6,*),' ntraj =',ntraj,' entering er2_range_fill'
      if (n .eq. 0) n = 1
      write(6,*)'er2 range fill'
      write(6,*)stlon,stlat,er2_range,nrings,npts
      do l = 1, nthp
        xn(n) = stlon
        yn(n) = stlat
        zn(n) = thp(l)
        n=n+1
      end do
      do nring = 1, nrings
        range = nring * er2_range / nrings
        call range_ring(range,bearing,xlatp,xlonp)
        do npt = 1, npts
          do l = 1, nthp
            xn(n)=xlonp(npt)
            yn(n)=xlatp(npt)
            zn(n) = thp(l)
            n=n+1
            if(n.ge.nmax)return
          end do
        end do
      end do
      ntraj=n
      write(6,*)' ntraj =',ntraj,' exiting er2_range_fill'
      return
      end
      subroutine range_ring(range,bearing,latp,lonp)
c
c   Routine based on range_ring.pro, by Paul Newman 93/02/25
c   Calculates the array of latitudes and longitudes
c   that are at a particular range from a central latitude
c   longitude site (forming a crcle around that point)
c INPUTS:   
c         stlat   = center latitude (in degs.)
c         stlon   = center longitude (in degs.)
c         range   = distance of the circle from the central point (in KM)
c         npts    = number of points in the latitude, longitude, distance
c                      arrays to be returned (if zero, defaults to 36)
c OUTPUTS:  
c         bearing = direction of point from the central point (0=N, 180=S, 
c                                                            270=W, 90=E)
c         latp    = an npts element vector of latitudes spaced 
c                   at del km increments
c         lonp    = corresponding longitudes for latp
c
      include 'comm_ukmo_3d_v20.'
      parameter (nprmax=36)
      real bearing(nprmax),latp(nprmax),lonp(nprmax)
      real sinBB(nprmax),cosBB(nprmax),sinAA(nprmax),cosAA(nprmax)
      real sinCC(nprmax),cosCC(nprmax)
      real b(nprmax),bb(nprmax),sinb(nprmax),cosb(nprmax)
      real lons
c
      pi=3.141592
c ** radius of the earth in radians
      re=20000./pi
      rad=180./pi
c
      if (abs(stlat) .gt. 90) then 
        print *,'range_ring: check your central latitude',stlat
        return
      endif
c  central longitude      
      lons=abs(stlon)

      if (npts .eq. 0) npts=36

      start_bear = 0.
      ccw = 1.0
      do n = 1, npts
        bearing(n) = ccw * (n-1)*360./npts + start_bear
        bb(n) = bearing(n)/rad
      enddo
c
c ** c is great circle angle between st and our points

      if (stlat .ge. 90) then
         sina=1.0e-10
         cosa=sqrt(1.-sina**2)
      endif
      if (stlat .le. -90) then
         sina=1.0e-10
         cosa=-1.0*sqrt(1.-sina**2)
      endif
      if ((stlat .gt. -90) .and. (stlat .lt. 90)) then
         a=(90.-stlat)/rad
         sina=sin(a)
         cosa=cos(a)
      endif
c
      c=range/re

      if (range .ge. 20000.) then 
        print *,'range_ring: Your range is too big',range
        range=19999.
        c=range/re
      endif

      sinc=sin(c)
      cosc=cos(c)

      do n = 1, npts
         cosBB(n) = cos(BB(n))
         sinBB(n) = sin(BB(n))
         cosb(n) = cosa*cosc+sina*sinc*cos(BB(n))
         b(n) = acos(cosb(n))
         sinb(n) = sin(b(n))
         latp(n) = 90. - b(n)*rad
c
c ** sincu and coscu are the arrays of sines and cosines of the longitudes

         sinAA(n) = sina * sinBB(n) /sinb(n)
         cosAA(n) = (cosa-cosc*cosb(n)) /sinc /sinb(n)
         sinCC(n) = sinc * sinBB(n) /sinb(n)
         cosCC(n) = - cosAA(n) * cosBB(n) + sinAA(n) * sinBB(n) * cosc
  
c ** calculate the lats and lons

         lonp(n)= atan2(sinCC(n),cosCC(n)) * rad + lons
      enddo
      return
      end
      SUBROUTINE FILL(DS,A,B,A1,B1,C1,D1,I,J,IM1,JM1,IFLAG,theta)
c
c*****************************************************************c
c                                                                 c
c   Routine fills in isopleths with air parcels                   c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      rad=6.37e3
      PI2 = 6.2831853071796
      dtr=pi2/360.
      A=ABS(A)
      B=ABS(B)
C
C  LOWER LEFT TRIANGLE 
C
       IF(IFLAG.EQ.1.OR.IFLAG.EQ.2) THEN
        X0=X(IM1)
        if (i .eq. 1) x0 = x0-360.
        Y0=Y(J)-B
        sx=pi2*rad*a*cos(y(j)*dtr)/360.
        sy=pi2*rad*b/360.
        s=sqrt(sx**2+sy**2)
        ntick=s/ds
        if(ntick.gt.0)then
        DX= A/ntick
        DY= B/ntick
C       WRITE(6,*)'  LOWER LEFT TRIANGLE, ntick, dx = ' ,ntick,dx 
        DO 1 JJ=1,NTICK
         NTRAJ=NTRAJ+1
         IF(NTRAJ.LE.nmax)THEN
          XN(NTRAJ)=X0
          YN(NTRAJ)=Y0
          ZN(NTRAJ)=theta
          X0=X0+DX
          Y0=Y0+DY
         ENDIF
1       CONTINUE
        endif
       RETURN
       ENDIF 
C
C  UPPER LEFT TRIANGLE 
C
       IF(IFLAG.EQ.3.OR.IFLAG.EQ.4) THEN
        X0=X(IM1)
        if (i .eq. 1) x0 = x0-360.
        Y0=Y(JM1)+B
        sx=pi2*rad*a*cos(y(jm1)*dtr)/360.
        sy=pi2*rad*b/360.
        s=sqrt(sx**2+sy**2)
        ntick=s/ds
        if(ntick.gt.0)then
        DX= a/ntick
        DY= b/ntick
C       WRITE(6,*)'  UPPER LEFT TRIANGLE, ntick, dx = ' ,ntick,dx 
        DO 2 JJ=1,NTICK
         NTRAJ=NTRAJ+1
         IF(NTRAJ.LE.NMAX)THEN
          XN(NTRAJ)=X0
          YN(NTRAJ)=Y0
          ZN(NTRAJ)=theta
          X0=X0+DX
          Y0=Y0-DY
         ENDIF
2       CONTINUE
        endif
       RETURN
       ENDIF 
C
C  UPPER RIGHT TRIANGLE 
C
       IF(IFLAG.EQ.5.OR.IFLAG.EQ.6) THEN
        X0=X(I)
        Y0=Y(JM1)+B
        sx=pi2*rad*a*cos(y(jm1)*dtr)/360.
        sy=pi2*rad*b/360.
        s=sqrt(sx**2+sy**2)
        ntick=s/ds
        if(ntick.gt.0)then
        DX= a/ntick
        DY= b/ntick
C       WRITE(6,*)'  UPPER RIGHT TRIANGLE, ntick, dx = ' ,ntick,dx 
        DO 3 JJ=1,NTICK
         NTRAJ=NTRAJ+1
         IF(NTRAJ.LE.NMAX)THEN
          XN(NTRAJ)=X0
          YN(NTRAJ)=Y0
          ZN(NTRAJ)=theta
          X0=X0-DX
          Y0=Y0-DY
         ENDIF
3       CONTINUE
        endif
       RETURN
       ENDIF 
C
C  LOWER RIGHT TRIANGLE 
C
       IF(IFLAG.EQ.7.OR.IFLAG.EQ.8) THEN
        X0=X(I)
        Y0=Y(J)-B
        sx=pi2*rad*a*cos(y(j)*dtr)/360.
        sy=pi2*rad*b/360.
        s=sqrt(sx**2+sy**2)
        ntick=s/ds
        if(ntick.gt.0)then
        DX= a/ntick
        DY= b/ntick
C       WRITE(6,*)'  LOWER RIGHT TRIANGLE, ntick, dx = ' ,ntick,dx 
        DO 4 JJ=1,NTICK
         NTRAJ=NTRAJ+1
         IF(NTRAJ.LE.NMAX)THEN
          XN(NTRAJ)=X0
          YN(NTRAJ)=Y0
          ZN(NTRAJ)=theta
          X0=X0-DX
          Y0=Y0+DY
         ENDIF
4       CONTINUE
        endif
       RETURN
       ENDIF 
C
C  RIGHT TRAPEZOID 
C
       IF(IFLAG.EQ.9.OR.IFLAG.EQ.10) THEN
        X0=X(IM1)+A
        if (i .eq. 1) x0 = x0-360.
        Y0=Y(JM1)
        sx=pi2*rad*(b-a)*cos(y(j)*dtr)/360.
cDD        sy=pi2*rad*(y(j)-y(jm1))/360.
        sy=pi2*rad*(y(j)-y0)/360.
        s=sqrt(sx**2+sy**2)
        ntick=s/ds
        if(ntick.gt.0)then
        DX= (b-a)/ntick
cDD        DY= (y(j)-y(jm1))/ntick
        DY= (y(j)-y0)/ntick
C       WRITE(6,*)'  RIGHT TRAPEZOID, ntick, dx = ' ,ntick,dx 
        DO 5 JJ=1,NTICK
         NTRAJ=NTRAJ+1
         IF(NTRAJ.LE.NMAX)THEN
          XN(NTRAJ)=X0
          YN(NTRAJ)=Y0
          ZN(NTRAJ)=theta
          X0=X0+DX
          Y0=Y0+DY
         ENDIF
5       CONTINUE
        endif
       RETURN
       ENDIF 
C
C  LOWER TRAPEZOID 
C
       IF(IFLAG.EQ.11.OR.IFLAG.EQ.12) THEN
        X0=X(IM1)
        if (i .eq. 1) x0 = x0-360.
        Y0=Y(J)-A
cDD        sx=pi2*rad*(x(i)-x(im1))*cos((y(j)-a)*dtr)/360.
        sx=pi2*rad*(x(i)-x0)*cos((y(j)-a)*dtr)/360.
        sy=pi2*rad*(a-b)/360.
        s=sqrt(sx**2+sy**2)
        ntick=s/ds
        if(ntick.gt.0)then
cDD        DX= (x(i)-x(im1))/ntick
        DX= (x(i)-x0)/ntick
        DY= (a-b)/ntick
C       WRITE(6,*)'  LOWER TRAPEZOID, ntick, dx = ' ,ntick,dx 
        DO 6 JJ=1,NTICK
         NTRAJ=NTRAJ+1
         IF(NTRAJ.LE.NMAX)THEN
          XN(NTRAJ)=X0
          YN(NTRAJ)=Y0
          ZN(NTRAJ)=theta
          X0=X0+DX
          Y0=Y0+DY
         ENDIF
6       CONTINUE
        endif
       RETURN
       ENDIF 
      RETURN
      END
      subroutine interp(u,v,q,uigw,vigw,xp,yp,zp)
c
c*****************************************************************c
c                                                                 c
c   Interpolate the velocity fields uigw,vigw to find             c
c   the velocity u,v for the point xp,yp,zp                       c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      uigw=0.
      vigw=0.
      u=0.
      v=0.
      q=0.
      xpold=xp
      ypold=yp
      zpold=zp
      if(xp.lt.x(1))xp=xp+360.
      nthw = nth 
c
c pole points
c
      lsp=0
      lnp=0
      if (yp.lt.y(1)) then
        lsp=1
        yp=y(1)
      endif 
      if (yp.gt.y(nr)) then
        lnp=1
        yp=y(nr)
      endif 
c
c top/bottom points
c
      lup=0
      ldn=0
      if (zp.ge.thw(1)) then 
        lup=1
        zp=thw(1)
      endif 
      if (zp.le.thw(nthw)) then
        ldn=1
        zp=thw(nthw)
      endif 
c
      do 40 l = 1, nthw-1
        lp1=l+1
        thl=thw(l)
        thlp1=thw(lp1)
        if((zp.le.thl).and.(zp.ge.thlp1))then
        zscale=(zp-thl)/(thlp1-thl)
        do 30 i=1,nc
        ip1=i+1
        if(i.eq.nc)ip1=1
        xlon=x(i) 
        xlonp1=x(ip1)
        if(i.eq.nc)xlonp1=360.+x(ip1)
        if((xp.ge.xlon).and.(xp.le.xlonp1))then
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
             ujp2=ugrd(jp1,i,lp1)
     +           +xscale*(ugrd(jp1,ip1,lp1)-ugrd(jp1,i,lp1))
             vjp2=vgrd(jp1,i,lp1)
     +           +xscale*(vgrd(jp1,ip1,lp1)-vgrd(jp1,i,lp1))
             qjp2=qgrd(jp1,i,lp1)
     +           +xscale*(qgrd(jp1,ip1,lp1)-qgrd(jp1,i,lp1))
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
                if(wmag.ge.wmax-10.) then
c                if(wmag.ge.45.) then
c what is 0 < theta < 2*pi for igwave routine? DF 10/16/97
                  call igwave(uigw,vigw,u,v,i,j,l,xp,yp,zp,wmag,theta) 
                  u=u+uigw
                  v=v+vigw
                endif
              endif
              goto 50
            endif
   20     continue
        endif
30      continue
        endif
40    continue
50    continue
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
      subroutine flight_path(theta)
c
c*****************************************************************c
c                                                                 c
c   Initialize along ER2 flight path or DIAL curtain              c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      real lat1,lon1,nlat,nlon
c     character*120 test
      PI2 = 6.2831853071796
      DTR = ASIN(1.0)/90. 
      RAD=6.37E6
      RADG = PI2 / 360.
      ds=2000.
      len=0
      nt=0
      nz=0
      n=1

      if(er2)then
      do leg=1,nleg                              
         olon=xleg0(leg)                        
         olat=yleg0(leg)              
         flon=xleg1(leg)                        
         flat=yleg1(leg)                        
         blat=.5*(olat+flat)          
         sx=pi2*rad*(flon-olon)*cos(flat*dtr)/360.
         sy=pi2*rad*(flat-olat)/360.
         s=sqrt(sx**2+sy**2)
         xn(n)=olon
	 yn(n)=olat
         zn(n)=theta
         iflag=0
         olon=xn(n)
         olat=yn(n)
         nstep=s/ds
         write(6,*)nstep,olat,olon,flat,flon,sx,sy,dx,dy
         if (nstep.gt.0) then 
             dx=(flon-olon)/nstep
             dy=(flat-olat)/nstep
             do 75 ISTEP=1,NSTEP
c
c  UPDATE TRAJECTORY
                xn(N+1)=olon+dx
                yn(N+1)=olat+dy
                zn(N+1)=theta

                IF (xn(N+1) .LT. 0.) xn(N+1)=360.+xn(N+1)
                IF (yn(N+1) .GT. 90.) THEN 
                    yn(N+1)=180.-yn(N+1)
                    xn(N+1)=180.+xn(N+1)
                ENDIF
                IF (yn(N+1) .LT. -90.) THEN 
                    yn(N+1)=-180.-yn(N+1)
                    xn(N+1)=180.+xn(N+1)
                ENDIF
                IF (xn(N+1) .GT. 360.) THEN 
                    MULT=xn(N+1)/360.
                    xn(N+1)=xn(N+1)-MULT*360.
                ENDIF
                n=n+1
                if(n.ge.nmax)return
	        olon=xn(n)
                olat=yn(n)
75           continue
         endif
         write(6,*)leg,n
      enddo
      endif

      ntraj=n
      return
      END
      subroutine igwave(uigw,vigw,u,v,i,j,l,xp,yp,zp,wmag,theta) 
c
c*****************************************************************c
c                                                                 c
c   Compute inertial gravity wave perturbation                    c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
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
     &  theta1=2.*pi-theta
      if((v.lt.0.).and.(u.lt.0.)) 
     &  theta1=theta+pi/2.
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
      dudy=ugrd(jm1,i,l)-ugrd(jp1,i,l)
      dvdx=vgrd(j,ip1,l)-vgrd(j,im1,l)
c      goto 10
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
10    continue
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
c temp0=3.*exp(-(wmax-wmag)**2/(wmax-70.)**2)
c
c temperature perturbation
c                              
c temp0=1.*exp(-(wmax-wmag)**2/(wmax-70.)**2)
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
     &     cos(2.*pi*(k*x1+l*y1-
     &           (omega+k*u+l*v)*t))
      uigw=k*omega*sin(2.*pi*(k*x1+l*y1-
     &                   (omega+k*u+l*v)*t))-
     &         l*f*cos(2.*pi*(k*x1+l*y1-
     &                   (omega+k*u+l*v)*t))
      vigw=l*omega*sin(2.*pi*(k*x1+l*y1-
     &                   (omega+k*u+l*v)*t))+
     &         k*f*cos(2.*pi*(k*x1+l*y1-
     &                   (omega+k*u+l*v)*t))
      temp=temp0*temp
      uigw=r*temp0*uigw/((2.*pi*omega)**2-f**2)
      vigw=r*temp0*vigw/((2.*pi*omega)**2-f**2)
      return
      end

      subroutine read_dial 
C
c*****************************************************************c
c                                                                 c
c   Read DIAL curtain data                                        c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      character*120 test
      PI2 = 6.2831853071796
      DTR = ASIN(1.0)/90. 
      RAD=6.37E6
      RADG = PI2 / 360.
      ds=2000.
      len=0
      lend=0
      nt=0
      nz=0
      test=dfile(1)
      do nf=1,119
         if(test(nf:nf+1).ne.' ')len=len+1
         if(dird(nf:nf+1).ne.' ')lend=lend+1
      enddo
c We need the DIAL date for DIAL initialisation        Duncan 7/1/99 
c
      OPEN(99,FILE=dird(1:lend)//test(1:len),status='old',
     &     READONLY,FORM='FORMATTED')
      print *,'opening ',dird(1:lend)//test(1:len)
      read(99,*)dial_date 
      print *,' dial_date =',dial_date 
      read(99,*)nt
      print *,'        nt =',nt  
      read(99,*)(xsave(k),k=1,nt),(ysave(k),k=1,nt),
     &          (tsave(k),k=1,nt)
      print *,' xsave ',(xsave(k),k=1,6)
      print *,' ysave ',(ysave(k),k=1,6)
      print *,' tsave ',(tsave(k),k=1,6)
      read(99,*) nt, nz
      print *,'   nt, nz  =',nt, nz   
      read(99,*)((o3save(k,l),k=1,nt),l=1,nz),
     &          (( psave(k,l),k=1,nt),l=1,nz),
     &          ((thsave(k,l),k=1,nt),l=1,nz),
     &          ((pvsave(k,l),k=1,nt),l=1,nz),
     &          (( usave(k,l),k=1,nt),l=1,nz),
     &          (( vsave(k,l),k=1,nt),l=1,nz),
     &          (( qsave(k,l),k=1,nt),l=1,nz),
     &        ((xmsfsave(k,l),k=1,nt),l=1,nz),
     &         ((qdfsave(k,l),k=1,nt),l=1,nz),
     &        ((xmrksave(k,l),k=1,nt),l=1,nz)
      if (dt .ge. 0.0) then               ! forward trajectory condition  
        idial = 1                         ! address first profile 
        ncount = nt 
      endif 
      if (dt .lt. 0.0) then               ! back trajectory condition 
        idial = nt                        ! address final profile 
        ncount = nt 
      endif 
      print *,' DIAL file read OK, ncount =',ncount 
      return
      end 
      subroutine read_dc8_annulus
C
c*****************************************************************c
c                                                                 c
c   Read DC-8 annulus data                                        c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
      character*120 test
      PI2 = 6.2831853071796
      DTR = ASIN(1.0)/90.
      RAD=6.37E6
      RADG = PI2 / 360.
      ds=2000.
      len=0
      lend=0
      nt=0
      nz=0
      nh=0
      nv=0
      test=dc8file(1)
      do nf=1,119
         if(test(nf:nf+1).ne.' ')len=len+1
         if(dirdc8(nf:nf+1).ne.' ')lend=lend+1
      enddo
c We need the DC-8 date for DC-8 initialisation        Duncan 7/1/99
c
      OPEN(99,FILE=dirdc8(1:lend)//test(1:len),status='old',
     &     READONLY,FORM='FORMATTED')
      print *,'opening ',dirdc8(1:lend)//test(1:len)
      read(99,*)dial_date
      print *,' dial_date =',dial_date
      read(99,*)nt
      print *,'        nt =',nt
      read(99,*)(tsave(k),k=1,nt)
c     print *,tsave
      read(99,*)nt,nh,nv
      print *,'  nt =',nt,'  nh =',nh,'  nv =',nv
      read(99,*)
     & ((( xsave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & ((( ysave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & ((( zsave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & ((( psave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & (((thsave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & (((pvsave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & ((( usave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & ((( vsave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & ((( qsave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & (((xmsfsave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & (((qdfsave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & (((xmrksave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & (((vpsave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv),
     & (((sfsave3d(k,l,n),k=1,nt),l=1,nh),n=1,nv)

      if (dt .ge. 0.0) then               ! forward trajectory condition
        idial = 1                         ! address first profile
        ncount = nt
      endif
      if (dt .lt. 0.0) then               ! back trajectory condition
        idial = nt                        ! address final profile
        ncount = nt
      endif
      print *,' DC-8 file read OK, ncount =',ncount
      return
      end

      subroutine init_dial(idial)
c
c*****************************************************************c
c                                                                 c
c   Initialise DIAL data profile # idial                          c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
c
c initialize x,y,p coincident with time, altitude curtain
c
      ist = ntraj+1 
      n = ist 
      do k = 1, nz
C include next 2 lines to exclude missing data points 
          if((o3save(idial,k).lt.99999.) .and.
     1       (psave(idial,k).lt.99999.)) then
           xn(n)=xsave(idial)
           yn(n)=ysave(idial)
           zn(n)=thsave(idial,k)
           pn(n)=psave(idial,k)
           pvn(n)=pvsave(idial,k)
           xmsfn(n)=xmsfsave(idial,k)
           qn(n)=qsave(idial,k)
           qdfn(n)=qdfsave(idial,k)
           xmrkn(n)=xmrksave(idial,k)
c Adding t0n and agen arrays                       Duncan 11/05/99
           t0n(n)=tsave(idial)
           agen(n)=0.0
           n = n+1 
          endif
      enddo
      ntraj = n-1
C
      return
      end

      subroutine init_dc8(idial)
c
c*****************************************************************c
c                                                                 c
c   Initialise DC-8 20x20 annulus at profile # idial              c
c                                                                 c
c*****************************************************************c
c
      include 'comm_ukmo_3d_v20.'
c
c initialize x,y,p coincident with time, annulus array
c
      ist = ntraj+1
      n = ist
      do j = 1, nhbin
      do k = 1, nvbin
           xn(n)=xsave3d(idial,j,k)
           yn(n)=ysave3d(idial,j,k)
           zn(n)=thsave3d(idial,j,k)
           pn(n)=psave3d(idial,j,k)
           pvn(n)=pvsave3d(idial,j,k)
           xmsfn(n)=xmsfsave3d(idial,j,k)
           qn(n)=qsave3d(idial,j,k)
           qdfn(n)=qdfsave3d(idial,j,k)
           xmrkn(n)=xmrksave3d(idial,j,k)
c Adding t0n and agen arrays                       Duncan 11/05/99
           t0n(n)=tsave(idial)
           agen(n)=0.0
           n = n+1
           if(n.ge.nmax)stop 'n exceeds nmax'
      enddo
      enddo
      ntraj = n-1

      return
      end

      subroutine zero_arrays
      include 'comm_ukmo_3d_v20.'
c
      do j = 1, nmax
c positional
        xn(j)=99999.
        yn(j)=99999.
        zn(j)=99999.
c dynamical/diagnostic
        pvn(j)=99999.
        pn(j)=99999.
        xmsfn(j)=99999.
        un(j)=99999.
        vn(j)=99999.
        qn(j)=99999.
        qdfn(j)=99999.
        xmrkn(j)=99999.
c Temporal quantities 
        agen(j)=99999.
        t0n(j)=99999.
c daylight summed quantities
        frday(j)=0.0
        frnght(j)=0.0
c minimum temperature 
        xmint(j)=99999.
c NO, CO, electrons
        xnon(j)=99999.
        xcon(j)=99999.
        xen(j)=99999.
      end do
      return
      end

