c***************************************************************************c
c Read ATTREX netcdf data                                                   c
c***************************************************************************c
      program read_attrex_nc
c
c     implicit none
      include "netcdf.inc"
c
      integer int,iret,ncid,nt
      parameter (int=100000)
      real Lon(int), Lat(int), Theta(int), PV(int), P(int), U(int)
      real Q(int), QDF(int), Mark(int), Geos_Theta(int), Temp(int)
      real Alt(int), V(int)
      character ncfile*53
c     character name*20
c     data name /'Dimension'/
      data ncfile /
     + '/Volumes/Data/ATTREX_data/Datfiles/ATTREX_20130205.nc'/

c open netcdf file w/o write privileges

      status=nf_open(ncfile,nf_nowrite,ncid)
      print *,'open status ',status,ncfile

c get dimensions
c
      status=nf_inq_dim(ncid, 1, name, nt)
      print *,'inq_dim ',status,nt

      status=nf_get_vara_real(ncid,1,1,nt,Lon)
      status=nf_get_vara_real(ncid,2,1,nt,Lat)
      status=nf_get_vara_real(ncid,3,1,nt,Theta)
      status=nf_get_vara_real(ncid,4,1,nt,PV)
      status=nf_get_vara_real(ncid,5,1,nt,P)
      status=nf_get_vara_real(ncid,6,1,nt,U)
      status=nf_get_vara_real(ncid,7,1,nt,V)
      status=nf_get_vara_real(ncid,8,1,nt,Q)
      status=nf_get_vara_real(ncid,9,1,nt,QDF)
      status=nf_get_vara_real(ncid,10,1,nt,Mark)
      status=nf_get_vara_real(ncid,11,1,nt,Geos_Theta)
      status=nf_get_vara_real(ncid,12,1,nt,Temp)
      status=nf_get_vara_real(ncid,13,1,nt,Alt)

c print variables to check that they are ok
  
c     print *,'Lon ',Lon(200:300)
      print *,'Lat ',Lat(200:300)
      print *,'Theta ',Theta(200:300)
      print *,'PV ',PV(200:300)

      status=nf_close(ncid)
C
      end
