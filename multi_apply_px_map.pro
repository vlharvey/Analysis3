;Applies px correction "Chris flat" to 1A data
;  prerequisite
;    A set of 1A files for the appropriate orbit(s) in /aim/data/cips/v3.12
;  input
;    orbits - scalar or array of orbits to process
;    px_map_orbit - scalar orbit to use as correction (must have a file ~/aim/px_map_<px_map_orbit>.sav)
;  output
;    a set of level 1A, 1B, and 4 data products with the Chris flat applied in directory ~/aim/data/cips/v3.12
;
; ****************************************
;  Warning! -- Potentially destructive!
;    Automatically deletes all existing data products in 
;    ~/aim/data/cips/v3.12 for the given orbit(s)
; ****************************************
pro multi_apply_px_map,orbits,px_map_orbit,graphics=graphics
  set_cips_production_vars
  inpath='/aim/data/cips/v3.12'
  outpath=string(getenv('HOME'),format='(%"%s/aim/data/cips/v3.15")')
  for i=0,n_elements(orbits)-1 do begin
    catch,err_status
    if err_status ne 0 then begin
      print,"Error occurred, skipping orbit ",orbits[i]
      i++
      if i ge n_elements(orbits) then return
    end
    ;Check if source files exist
    search=string(inpath,orbits[i],format='(%"%s/cips_sci_1a_orbit_%05d_*")')
    junk=file_search(search,count=count)
    if count lt 4 then begin
      print,"Not enough files for orbit ",orbits[i]
      print,search
      print,junk
    end else begin
      ;Get rid of existing data products for this orbit
      cmd=string(outpath,orbits[i],format='(%"rm -v %s/cips_sci_*_orbit_%05d_* ")')
      print,cmd
      spawn,cmd,result
      print,result

      ;Link all the original 1A files
      cmd=string(inpath,orbits[i],outpath,format='(%"ln -sv %s/cips_sci_1a_orbit_%05d_* %s")')
      print,cmd
      spawn,cmd,result
      print,result

      ;Remove the link for the PX camera
      cmd=string(outpath,orbits[i],format='(%"rm -v %s/cips_sci_1a_orbit_%05d_*px*")')
      print,cmd
      spawn,cmd,result
      print,result

      ;Run smooth_map to get new PX camera file
      smooth_map_px,orbits[i],px_map_orbit,inpath,outpath,graphics=graphics

      ;Rename all 1A files to v3.15
      cmd=string(outpath,orbits[i],format='(%"(cd %s; for i in cips_sci_1a_orbit_%05d_*.11.nc; do mv -v $i `basename $i .11.nc`.15.nc; done)")')
      print,cmd
      spawn,cmd,result
      print,result

      ;Run it up to level 4
      l4=run_level_4(orbits[i])
      if keyword_set(graphics) then begin
        device,retain=2
        plot,yrange=[0.99,1.01],psym=3,*l4.zenith_angle_ray_peak,*l4.indicators.ratall
      end
      catch,/cancel
    end
  end
end
