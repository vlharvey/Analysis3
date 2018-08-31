function doy2dfs, doy,year,hem

  ; This function converts day of year (doy) and year to day from solstice (DFS).
  ; call it like this: dfs = doy2dfs(doy,year,hem)
  ; It assumes a constant date for solstice: NH: 06/21, SH: 12/21.
  ; I deals with NaNs in the doy array.
  ; Changed from doy2dfs_old.pro on 20091022 by Susanne Benze.

  ; doy  expects an array  of any type (may switch years), e.g., [363,364,365,1,2,3]
  ; year expects one value of any type: NH07 -> year=2007, SH07/08 -> year=2007, SH08/09 -> year=2008, and so on.
  ; hem  expects one value of string, 'nh' or 'sh'
  
  y = long(year)  ; to avoid change of variable type. TYPE is given back to main code.
  h = strlowcase(hem)
  d = intarr(n_elements(doy))*!values.f_nan
  xx = where(finite(doy) eq 1,nxx)
  if (nxx ne 0) then d[xx] = fix(doy[xx])
  
  if (size(h,/type) ne 7) then begin
    on_error,1
    message, 'hem is not a string - please input a string!'
  endif
  if (n_elements(h) gt 1) then begin
    on_error,1
    message, 'hem has more than 1 element - please input one value only!'
  endif
  if (n_elements(y) gt 1) then begin
    on_error,1
    message, 'year has more than 1 element - please input one value only!'
  endif
  
  if (h eq 'nh') then soldate = long([strcompress(string(y),/remove_all)+'0621'])
  if (h eq 'sh') then soldate = long([strcompress(string(y),/remove_all)+'1221'])
  soldoy = fix((date2doy(soldate))[0])
  
  ; check for NaNs
  ;;;;;;;;;;;;;;;;
  dfs = intarr(n_elements(d))*!values.f_nan
  
  z = where(finite(d) eq 1,nz)
  if (nz ne 0) then begin
    zdfs = dfs[z]
    zd = d[z]
    case h of
      'nh': begin
        zdfs = zd-soldoy
      end
      'sh': begin
        zdfs = zd-soldoy
        x = where(zdfs lt 0 and zd lt 172,nx)
        if (nx ne 0) then zdfs[x] = zdfs[x]+365+1*leap(y)
      end
    endcase
  endif
  dfs[z] = zdfs
  
  return,dfs
end