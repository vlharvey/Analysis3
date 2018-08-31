;
; Check Meta data for rev_05
;
restore,'read_cips_file.sav'
spawn,'ls /Volumes/Data/CIPS_data/Datfiles/cips_sci_2_orbit_*2011-0*r05_psf.nc.gz',ifiles
spawn,'ls /Volumes/Data/CIPS_data/Datfiles/cips_sci_3a_2011*r05.nc.gz',ifiles
for n=0L,n_elements(ifiles)-1L do begin
    DATA=READ_CIPS_FILE(ifiles(n))
help,/str,data
stop
endfor

END

