PRO wave1, X, A, F, pder

  F = A[0] * sin(2*!pi/360.*x + A[1]) + A[2]

  IF N_PARAMS() GE 4 THEN begin ; If the procedure is called with four parameters, calculate the partial derivatives.
      pder = FLTARR(N_ELEMENTS(X), 3)  
      ; Compute the partial derivatives with respect to  
      ; a0 and place in the first row of PDER:  
      pder[*, 0] = [sin( 2*!pi/360.* x + A[1]  )]  
      ; Compute the partial derivatives with respect to  
      ; a1 and place in the second row of PDER:  
      pder[*, 1] =   A[0] * cos(  2*!pi/360.*x +A[1])    
      pder[*, 2] =   1
	endif

END