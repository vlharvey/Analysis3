function kgmt,imn,idy,iyr,iday

; --- This function computes the Julian day number (GMT) from the
; --- day, month, and year information.

mday=[31,28,31,30,31,30,31,31,30,31,30,31]
kgmt=0
ldays=0
leapyr=(iyr mod 4)
if leapyr eq 0 then begin
   leapdy=1 
endif else begin
   leapdy=0
endelse

if imn eq 1 then goto, jump

for i=0,imn-2 do begin
   ldays=ldays+mday(i)
endfor

jump: if leapdy eq 1 then begin
         if ldays lt 59 then leapdy = 0
      endif 

iday=ldays+idy+leapdy

end
