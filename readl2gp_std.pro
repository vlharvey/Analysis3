; $Id: readl2gp_std.pro,v 1.5 2007/07/13 20:22:00 fullerr Exp $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; <pre>
; NAME:
;   ReadL2GP_STD
;
; PURPOSE:
;   This funtion returns a structure containing the information in an
;   EOS-Aura Microwave Limb Sounder (MLS) Level 2 Geophysical Product
;   (L2GP).  The MLS L2GP file must be in HDF-EOS 5 format, and only
;   swath in the dataset can be read at once.
; 
;   The first (often only) swath in the file is returned unless the user
;   supplies a value for swathName. In either case, the name of the
;   swath read is returned into swathName.
; 
;   Error checking has been omitted in favor of clarity.  Also note
;   increased complexity due to IDL lacking an HDF-EOS version 5 library.
; 
; AUTHORS:
;   MLS Science Team: 
;   Nathaniel Livesey, Ph.D. E-mail: livesey@mls.jpl.nasa.gov
;   Ryan Fuller              E-mail: fullerr@mls.jpl.nasa.gov
;
; CALLING SEQUENCE:
;   data = ReadL2GP_STD(filename, swathName=swathName)
;
; REQUIRES:
;   IDL 6.1 or Greater
;
; INPUT PARAMETERS:
;   filename      - A scalar string that indicates the MLS L2GP file
;                   to be read (in HDF-EOS 5)
; 
; KEYWORD PARAMETERS
;   swathName     - A scalar string that is the name of the HDF-EOS
;                   swath to read.  If not supplied, the first swath
;                   found in the file will be used.
;   variableName  - A scalar string that is the name of the variable
;                   within the HDF-EOS swath to read.  By default it
;                   is 'l2gpValue'. 
;   precisionName - A scalar string that is name of the variable
;                   within the HDF-EOS swath to read.  By default it
;                   is 'l2gpValuePrecision'. It is ignored if
;                   variableName is not supplied.
;   status        - The exit status of the program.  1=success,
;                   0=failure.  Any values given will be overwritten
;                   by the program.
; RETURN VALUE:
;   If successful, a data structure containing the following fields
;   are returned: swathName, number of sample times (nTimes), number
;   of vertical levels (nLevels), number of frequencies (nFreqs),
;   pressure, frequency, latitude, longitude, time, localSolarTime,
;   solarZenithAngle, lineOfSightAngle, orbitGeodeticAngle,
;   chunkNumber, l2gpValue, l2gpPrecision, status, quality,
;   attributes.  If the any of these values are not found in the
;   file.  They are replaced by a scalar value of the proper datatype.

;   If failure, the program returns a 0 (this is also indicated by the
;   keyword status equals 0.
;
; MODIFICATION HISTORY:
;   Written by: Ryan Fuller, MLS Science Team, July 2005
;   $Revision: 1.5 $ $Date: 2007/07/13 20:22:00 $
;   $Log: readl2gp_std.pro,v $
;   Revision 1.5  2007/07/13 20:22:00  fullerr
;   Bug Fix
;
;   Revision 1.4  2007/06/21 16:27:18  fullerr
;   Updated documentation for open channel
;
;   Revision 1.3  2007/05/22 16:53:51  fullerr
;   Added support for the Theta vertical coordinate to be present in MLS DMP files
;
;   Revision 1.2  2007/05/21 16:16:03  fullerr
;   Added convergence fixed incorrect setting of nLevels
;
;   Revision 1.1  2005/07/12 12:43:09  fullerr
;   Initial Revision
;
; 
; Copyright 2007, 2005, by the California Institute of Technology. 
; ALL RIGHTS RESERVED. United States Government Sponsorship
; acknowledged. Any commercial use must be negotiated with the Office
; of Technology Transfer at the California Institute of Technology.
;
; This software may be subject to U.S. export control laws. By
; accepting this software, the user agrees to comply with all
; applicable U.S. export laws and regulations. User has the
; responsibility to obtain export licenses, or other export authority
; as may be required before exporting such information to foreign
; countries or providing access to foreign persons. 
; </pre>
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
FUNCTION ReadL2GP_Std, filename, swathName=swathName, status=status, $
                       variableName=variableName, $
                       precisionName=precisionName

  COMPILE_OPT IDL2
  
  ;; Set up status to be a failed run.  It will be set to success just
  ;; before the end.
  status = 0

  ;; Make sure the file argument is given
  IF NOT Keyword_Set(filename) THEN BEGIN
    Print, 'USAGE - filename must be given!'
    RETURN, 0
  ENDIF ELSE filename = filename[0]

  ;; First make sure the file exists.  If not print, a warning message
  ;; and return a 0.
  OpenR, unit, filename, /GET_LUN, error=err
  IF err NE 0 THEN BEGIN
    Print, 'ERROR - filename not found.'
    RETURN, 0 
  ENDIF ELSE Free_LUN, unit

  ;; Now make sure the file is HDF 5
  IF H5F_Is_HDF5(filename) EQ 0 THEN BEGIN
    Print, 'ERROR - Supplied file is not HDF5'
    RETURN, 0
  ENDIF

  ;; Setup default variables within the HDF-EOS swath structure
  IF N_Elements(variableName) EQ 0 THEN BEGIN
    variableName = 'L2gpValue'
    precisionName = 'L2gpPrecision'
  ENDIF ELSE BEGIN
    variableName = variableName[0]
    precisionName = (N_Elements(precisionName) NE 0 ? $
                     precisionName[0] : variableName + 'Precision')
  ENDELSE

  ;; If this is an hdf 5 file open it that way
  ;; This is an hdf5 file
  fileID = H5F_Open(filename)

  ;; Query the swaths group
  groupName = 'HDFEOS/SWATHS'
  noSwaths = H5G_Get_NMembers(fileID, groupName)
  IF noSwaths LE 0 THEN BEGIN
    H5F_Close, fileID
    Print, 'ERROR - No Swaths in supplied file!'
    RETURN, 0
  ENDIF

  ;; Identify the swath we want
  swathIndex = 0
  IF N_Elements(swathName) NE 0 THEN BEGIN
    swathName = swathName[0]
    REPEAT BEGIN
      thisName = H5G_Get_Member_Name(fileID, groupName, swathIndex)
      swathIndex = swathIndex + 1
    ENDREP UNTIL swathIndex EQ noSwaths OR thisName EQ swathName
    IF thisName NE swathName THEN BEGIN
      H5F_Close, fileID
      Print, 'ERROR - No Swath: ' + swathName + ' in file.'
      RETURN, 0
    ENDIF
  ENDIF ELSE swathName = H5G_Get_Member_Name(fileID, groupName, 0)

  ;; Now attach to this group
  swathID = H5G_Open(fileID, groupName + '/' + swathName)

  ;; Get the geolocation fields
  ;; First open the group and get all of its members...
  gField = 'Geolocation Fields'
  geoLocID = H5G_Open(swathID, gField)
  nMembers = H5G_Get_NMembers(swathID, gField)
  IF nMembers GT 0 THEN BEGIN
    members = StrArr(nMembers)
    FOR i = 0, nMembers - 1 DO BEGIN
      members[i] = H5G_Get_Member_Name(swathID, gField, i)
    ENDFOR
  ENDIF ELSE members = ''

  ;; .. Now read in the base set of geolocation variables.
  dsID = H5D_Open(geoLocID, 'Time')
  time = H5D_Read(dsID)
  H5D_Close, dsID
  dsID = H5D_Open(geoLocID, 'Latitude')
  latitude = H5D_Read(dsID)
  H5D_Close, dsID
  dsID = H5D_Open(geoLocID, 'Longitude')
  longitude = H5D_Read(dsID)
  H5D_Close, dsID
  ;; Sort out the dimensions of time
  nTimes = N_Elements(time)

  ;; Now we will check and see if one of the Geolocation fields
  ;; normally found in the MLS L2GP files is in the given file.  If
  ;; so, we assume that all will be there.  Looking for this can
  ;; hopefully make this program usable for datasets like HIRDLS.
  dummy = Where(members EQ 'ChunkNumber', cnt)
  IF cnt GT 0 THEN BEGIN
    dsID = H5D_Open(geoLocID, 'ChunkNumber')
    chunkNumber = H5D_Read(dsID)
    H5D_Close, dsID
    dsID = H5D_Open(geoLocID, 'LineOfSightAngle')
    lineOfSightAngle = H5D_Read(dsID)
    H5D_Close, dsID
    dsID = H5D_Open(geoLocID, 'LocalSolarTime')
    localSolarTime = H5D_Read(dsID)
    H5D_Close, dsID
    dsID = H5D_Open(geoLocID, 'OrbitGeodeticAngle')
    orbitGeodeticAngle = H5D_Read(dsID)
    H5D_Close, dsID
    dsID = H5D_Open(geoLocID, 'SolarZenithAngle')
    solarZenithAngle = H5D_Read(dsID)
    H5D_Close, dsID
  ENDIF ELSE BEGIN
    ;; Give null placeholders to all of those data fields so
    ;; the return field works properly
    chunkNumber        = 0L
    lineOfSightAngle   = 0.0
    localSolarTime     = 0.0
    orbitGeodeticAngle = 0.0
    solarZenithAngle   = 0.0
  ENDELSE

  ;; Also get the extra dimensional data (ie the ones that do not have
  ;; nProfiles data elements).
  dummy = Where(members EQ 'Pressure', cnt)
  IF cnt GT 0 THEN BEGIN
    dsID = H5D_Open(geoLocID, 'Pressure')
    pressure = H5D_Read(dsID)
    H5D_Close, dsID
    nLevels = N_Elements(pressure)
  ENDIF ELSE BEGIN
    nLevels = 1
    pressure = [0.0]
  ENDELSE
  dummy = Where(members EQ 'Frequency', cnt)
  IF cnt GT 0 THEN BEGIN
    dsID = H5D_Open(geoLocID, 'Frequency')
    frequency = H5D_Read(dsID)
    H5D_Close, dsID
    nFreqs = N_Elements(frequency)
  ENDIF ELSE BEGIN
    nFreqs = 1
    frequency = [0.0]
  ENDELSE 
  dummy = Where(members EQ 'Theta', cnt)
  IF cnt GT 0 THEN BEGIN
    dsID = H5D_Open(geoLocID, 'Theta')
    theta = H5D_Read(dsID)
    H5D_Close, dsID
    nTheta = N_Elements(frequency)
  ENDIF ELSE BEGIN
    nTheta = 1
    theta = [0.0]
  ENDELSE 

  ;; We have now read all desired geolocation fields
  H5G_Close, geoLocID

  ;; Get the data fields
  ;; First open the group and get all of its members will come in
  ;; handy when looking for status/quality
  gField = 'Data Fields'
  dataID = H5G_Open(swathID, gField)
  nMembers = H5G_Get_NMembers(swathID, gField)
  IF nMembers GT 0 THEN BEGIN
    members = StrArr(nMembers)
    FOR i = 0, nMembers - 1 DO BEGIN
      members[i] = H5G_Get_Member_Name(swathID, gField, i)
    ENDFOR
  ENDIF ELSE members = ''

  ;; These two are always members.  We also want the attributes for
  ;; the variableName.
  atts = ''
  dsID = H5d_Open(dataID, variableName)
  l2gpValue = H5D_Read(dsId)
  FOR i = 0, H5A_Get_Num_Attrs(dsId) - 1 DO BEGIN
    attId = H5A_Open_Idx(dsId, i)
    atts = i EQ 0 ? Create_Struct(H5A_Get_Name(attId), (H5A_Read(attId))[0]) : $
                    Create_Struct(atts, H5A_Get_Name(attId), (H5A_Read(attId))[0])
    H5A_Close, attId
  ENDFOR
  H5D_Close, dsId
  dsID = H5d_Open(dataID, precisionName)
  l2gpPrecision = H5D_Read(dsId)
  H5D_Close, dsId  

  ;; Get status, quality, and convergence if present.  This resides in the data
  ;; fields portion of the swath.
  dummy = Where(members EQ 'Status', cnt)
  IF cnt GT 0 THEN BEGIN
    dsID   = H5D_Open(dataID, 'Status')
    status = H5D_Read(dsID)
    H5D_Close, dsID
    dsID    = H5D_Open(dataID, 'Quality')
    quality = H5D_Read(dsID)
    H5D_Close, dsID
    dummy = Where(members EQ 'Convergence', cnt)
    IF cnt GT 0 THEN BEGIN
      dsID   = H5D_Open(dataID, 'Convergence')
      convergence = H5D_Read(dsID)
      H5D_Close, dsID
    ENDIF ELSE convergence = 0.0 ; A placeholder
  ENDIF ELSE BEGIN
    ;; If not given give null placeholders for status and quality
    status      = 0L
    quality     = 0.0
    convergence = 0.0
  ENDELSE

  ;; We are done accessing the data fields portion of the swath
  H5G_Close, dataID

  ;; Close group, file
  H5G_Close, swathID
  H5F_Close, fileID

  ;; Now construct the result
  result = {swathName:swathName, $
            nTimes:nTimes, $
            nLevels:nLevels, $
            nFreqs:nFreqs, $
            nTheta:nTheta, $
            $
            pressure:pressure, $
            frequency:frequency, $
            theta:theta, $
            $
            latitude:latitude, $      
            longitude:longitude, $       
            time:time, $              
            localSolarTime:localSolarTime, $    
            solarZenithAngle:solarZenithAngle, $
            lineOfSightAngle:lineOfSightAngle, $      
            orbitGeodeticAngle:orbitGeodeticAngle, $    
            chunkNumber:chunkNumber, $
            $
            l2gpValue:l2gpValue, $
            l2gpPrecision:l2gpPrecision, $
            status:status, $
            quality:quality, $
            convergence:convergence, $
            attributes:atts $
           }

  ;; That's it.  Set the stauts to a successful run and return
  status = 1
  RETURN, result
END
