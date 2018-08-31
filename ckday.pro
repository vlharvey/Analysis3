pro ckday,kday,kyr

; --- This routine changes the Julian day from 365(6 if leap yr)
; --- to 1 and increases the year, if necessary.
;
if kday eq 366 then begin
   if (kyr mod 4) eq 0 then begin
       return
   endif else begin
       kday = 1
       kyr = kyr + 1
   endelse
endif

if kday eq 367 then begin
   kday = 1
   kyr = kyr + 1
endif

end

