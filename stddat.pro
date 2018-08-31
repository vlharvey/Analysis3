function stddat,imn,iday,iyr,refdate

; --- Determines the number of days since Jan 1, 1956.

month=[31,28,31,30,31,30,31,31,30,31,30,31]
ibase=1956
leapdays=0

; --- Compute number of days since 1 January 1956
idays = float(iyr-ibase)*365.

; --- test for leap year
; --- add the extra days for the period from 1956
if (iyr mod 4) eq 0 then begin
   month(1) = 29
   leapdays = fix(float(iyr-1956)/4. )
endif else begin
   month(1) = 28
   leapdays = fix(float(iyr-ibase)/4.) + 1 
endelse

if imn le 1 then goto, jump

for j = 0, imn-2 do begin
    idays = idays + month(j)
endfor

jump: refdate = iday + idays + leapdays

end


