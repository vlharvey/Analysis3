function open_eps, filename=filename

  if n_elements(filename) eq 0 then filename='~/plot.ps'

  set_plot,'ps'                 ;Save plot as postscript
                                ;the device command is used to configure postscript
                                ; below are some parameters I generally use
                                ; In IDL a "/some_quantity" => some_quantity=1
  device,/bold,/color,filename=filename


  !p.font = 0                   ;set the IDL font system variable to 0 for ps fonts

end
