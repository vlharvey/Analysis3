
ncfile='/Volumes/cloud/data/WACCM_data/Input_data/spectral_irradiance_Lean_1950-2014_daily_GOME-Mg_Leap_c150623.nc'
		
		;Open the *.nc file
		 ncid=ncdf_open(ncfile)
print,ncfile
		 result=ncdf_inquire(ncid)   ;Inquire about the data
		 nvars=result.nvars        ;# variables in the file
		 ;Read in the data
		for ivar=0,nvars-1 do begin
				result=ncdf_varinq(ncid,ivar) ;get the data name, type, dimensions
				;Puts data into array called "data" and variable name into "result.name":
				ncdf_varget,ncid,ncdf_varid(ncid,result.name),data
			    
			 ; Invoke some magic to check for string data
			 ; masquerading as byte data, but don't convert
			 ; byte data blindly, i.e., quality_flags is a 2-dimensional
			 ; array of byte data. 
			 ; (This is done to account for a bug in the ncdf write routine.)
				  if ( ( size( data, /n_dimensions )  EQ 1 ) && $
				    ( size( data, /type ) EQ 1 ) ) then $
				    data = string( data )
			
help,ivar,data
			 ;Extract each variable from the "data" structure and name it
			;  the corresponding "name" from "result.name":
			       if Execute(result.name + ' = data') eq 0 then $
			       Print, ' "Execute" command failed -- are you in Virtual Machine mode?'            
		endfor
		       PRINT, RESULT.NAME
		       NCDF_CLOSE, ncid
;
; save an IDL file
;
save,file='spectral_irradiance_Lean_1950-2014_daily_GOME-Mg_Leap_c150623.sav',BAND_WIDTH,DATE,ssi,ssi_ref,tsi,tsi_ref,wavelength,REF_TIME_BOUND
		       
end
