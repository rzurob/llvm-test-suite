!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 29, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : ENCODING= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of incorrect use of
!*                               ENCODING= specifier in I/O statements at
!*                               compile time
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num, encoding_mode_num

  character(20) :: encoding_mode = 'default'

  !only character expressions are allowed for the ENCODING= specifier
  !in open statements

  open (UNIT=2, FILE='encodingspecifier.dat', ENCODING=encoding_mode_num)

  open (UNIT=2, FILE='encodingspecifier.dat', ENCODING=encoding_mode)

  !only character variables are allowed for the ENCODING= specifer
  !in inquire statements

  inquire(2, encoding=encoding_mode_num)

  close(2)

end program
