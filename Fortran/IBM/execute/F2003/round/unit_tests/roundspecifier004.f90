!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of the ROUND= specifier
!*                               in I/O statements at run time with
!*                               unformatted I/O
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num

  character(20) :: form_mode = 'unformatted'
  character(20) :: format_descrip = '(f7.4 , rc )'

  !ROUND= specifier not allowed in unformatted I/O

  open(UNIT=2,FILE='real4.dat', FORM=form_mode, ROUND='down')

  read(UNIT=2, FMT='(f7.4)',ROUND='nearest') num

  write(2,FMT='(f7.4)',ROUND='zero') num

  close(2)


  open(UNIT=2,FILE='real4.dat', ROUND='down', FORM = form_mode)

  read(2,format_descrip) num

  write(2,format_descrip) num

  print format_descrip, num

end program


