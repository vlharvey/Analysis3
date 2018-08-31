PRO MKLTIME,TIME,LON,LTIME,lday

;CALCULATE LOCAL TIME.
;TIME IS THE UT TIME IN HOURS.  LON IS THE EAST LONGITUDE.

;LOCAL TIME:

; THE TIME DIFFERENCE FROM UTC (UTH OR UT OR GMT), dH, IS THE EAST
;   LONGITUDE MULTIPLIED BY 24 HOURS/360 DEGREES.  0 LONGITUDE IS UTC, 180
;   LONGITUDE IS INTERNATIONAL DATE LINE.  THEREFORE, THE LOCAL TIME CAN
;   BE CALCULATED AS:
;   0 LE LON LE 180:  LTIME=UT+dH.    IF LT>24 THEN LT=LT-24 AND LOCAL
;       DAY=DAY+1.
;   180 LT LON LT 360:  LTIME=UT-dH.  IF LT<0  THEN LT=LT+24 AND LOCAL
;       DAY=DAY-1.

;UT TIME (HRS) = SEC/3600.
UTH=TIME
LTIME=TIME

   L=LON
   X=WHERE(L GT 180,NX)
   IF NX GT 0 THEN L(X)=360-L(X)

   X=WHERE(LON LE 180,NX)
   IF NX GT 0 THEN LTIME(X)=UTH(X)+L(X)*24./360.
   X=WHERE(LON GT 180,NX)
   IF NX GT 0 THEN LTIME(X)=UTH(X)-L(X)*24./360.

   X=WHERE(LTIME GT 24,NX)
   IF NX GT 0 THEN BEGIN
      LTIME(X)=LTIME(X)-24
      lday(x)=lday(x)+1
   ENDIF

   X=WHERE(LTIME LT 0,NX)
   IF NX GT 0 THEN BEGIN
      LTIME(X)=LTIME(X)+24
      lday(x)=lday(x)-1
   ENDIF

END