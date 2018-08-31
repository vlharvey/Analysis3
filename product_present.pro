pro product_present,first,last
  x=indgen(last-first+1)+first
  plot,[0,1],[0,1],yrange=[-1,15],/ys,xrange=[first,last],/nodata
  present=bytarr(16,n_elements(x))
    l1a_px="cips_sci_1a_orbit_[0-9][0-9][0-9][0-9][0-9]_*px*.nc"
    l1a_mx="cips_sci_1a_orbit_[0-9][0-9][0-9][0-9][0-9]_*mx*.nc"
    l1a_py="cips_sci_1a_orbit_[0-9][0-9][0-9][0-9][0-9]_*py*.nc"
    l1a_my="cips_sci_1a_orbit_[0-9][0-9][0-9][0-9][0-9]_*my*.nc"
    l1b="cips_sci_1b_orbit_[0-9][0-9][0-9][0-9][0-9]_*.nc"
    l1c="cips_sci_1c_orbit_[0-9][0-9][0-9][0-9][0-9]_*.nc"
    l4="cips_sci_4_orbit_[0-9][0-9][0-9][0-9][0-9]_*.nc"
    l4cvo="cips_sci_4_cvo_orbit_[0-9][0-9][0-9][0-9][0-9]_*.nc"
    l2a="cips_sci_2a_orbit_[0-9][0-9][0-9][0-9][0-9]_*1.nc"
    l2a_a="cips_sci_2a_orbit_[0-9][0-9][0-9][0-9][0-9]_*_asc.nc"
    l2a_d="cips_sci_2a_orbit_[0-9][0-9][0-9][0-9][0-9]_*_dsc.nc"
    dp=[l1a_px,l1a_mx,l1a_py,l1a_my,l1b,l1c,l4,l4cvo,l2a,l2a_a,l2a_d]
    dp2=['l1a_px','l1a_mx','l1a_py','l1a_my','l1b','l1c','l4','l4cvo','l2a','l2a_a','l2a_d']
    for j=0,n_elements(dp)-1 do begin
      xyouts,100,j,dp2[j],charsize=3
      junk=file_search('/aim/data/cips/v1.1/'+dp[j],count=count)
      for i=0,n_elements(junk)-1 do begin
        w=strpos(junk[i],'orbit_')
        k=fix(strmid(junk[i],w+6,5))
        oplot,[k,k],[j,j],psym=1
        present[j,k]=count
      end
    end
end
