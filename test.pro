fname='/aura7/harvey/CIPS_data/Datfiles/cips_sci_4_orbit_01434_2007-212_v03.20.nc'
restore,'read_cips_file.sav
data=read_cips_file(fname,/full_path,attributes=attributes)
end
