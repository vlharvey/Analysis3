pro kdate,gmt,kyr,kmn,kdy 
;
; ---  This routine gives back kmn,kdy information from the Julian day #.
;
month=[31,28,31,30,31,30,31,31,30,31,30,31]
i    = 0
kmn  = 0
igmt = fix(gmt)
if (kyr mod 4) eq 0 then month(1) = 29 else month(1) = 28

for j = 0, 11 do begin
    i = i + month(j)
    if i gt igmt then begin
       kmn = j+1
       kdy = igmt + month(j) - i
       goto, jump
    endif 
    if i eq igmt then begin
       kmn = j+1
       kdy = month(j)
       goto, jump
     endif
endfor

; --- In the event that this is the first day of the new year (366/7)
; --- then change the month and day to 1
if igmt eq 366 then begin
   if (kyr mod 4) ne 0 then begin
       kdy = 1
       kmn = 1
       return
   endif
endif
if igmt eq 367 then begin
   kdy = 1
   kmn = 1
endif
 
jump:

end

