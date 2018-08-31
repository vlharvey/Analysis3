PRO mie_phase_cor, l1b, p_size_nm, debug=debug

;---------------------------------------------------------------------------------------
;
; mie_phase_cor.pro is a procedure used to devide the
; albedo array by the mie phase function.  The procedure:
;
;  ( COS( view angle ) * albedo_array ) / ( mie phase function )
;
;  The images pointed to by l1b.albedo are replaced by the newly modified images
;
;	INPUT:
;		l1b data structure
;		particle size in nm
;		debug - an optional keyword used to generate images
;
;	OUTPUT:
;		l1b data structure (mogified)
;
;  Written by:
;  Brian Templeman
;---------------------------------------------------------------------------------------

  IF KEYWORD_SET(debug) THEN BEGIN
        !p.MULTI=[0,0,2]
        DEVICE, DECOMPOSED = 0, RETAIN = 2

        WINDOW, 0, XSIZE = 500, YSIZE = 480
        loadct, 3
        !P.BACKGROUND = 0
  ENDIF

  ; Get the albedo frame
  albedo_drf     = dereference_single(l1b.albedo)
  num_scenes     = N_ELEMENTS(albedo_drf)
  mie_image_arr  = PTRARR(num_scenes)

  FOR scene = 0, num_scenes - 1 DO BEGIN

     scene_image=*(albedo_drf[scene])

     ; Get the viewing angle data and perform COS function
     temp_drf=dereference_single(l1b.view_angle_ray_peak)
     cos_view_angle=*(temp_drf[scene]) * !DTOR
     cos_view_angle = COS(cos_view_angle)

     ; Get the scattering angle
     temp_drf=dereference_single(l1b.scattering_angle)
     scattering_angle=*(temp_drf[scene])

     ; Calculate the MIE phase
     mie_phase_cor = mie_phase(scattering_angle, p_size_nm)
  
     tmp_image = (scene_image * cos_view_angle) / mie_phase_cor
     mie_image_arr[scene] = PTR_NEW( tmp_image )

     IF KEYWORD_SET(debug) THEN BEGIN
        ERASE
        TVSCL, scene_image, 0, /NaN
        TVSCL, dereference_single(mie_image_arr[scene]), 1, /NaN

        XYOUTS, 85, 450, "Original", COLOR=255, CHARSIZE = 2, /DEVICE
        XYOUTS, 85, 100, "Modified", CHARSIZE=2, COLOR=255, /DEVICE  
        XYOUTS, 235, 100, STRING(p_size_nm) + " nm", CHARSIZE=2, COLOR=255, /DEVICE
      ENDIF
  ENDFOR
  STR_FREE, l1b.albedo
  l1b.albedo=PTR_NEW( mie_image_arr )
END
