!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 30, 2007
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ENCODING= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of incorrect use of
!*                               ENCODING= specifier in I/O statements at
!*                               run time
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num, encoding_mode_num

  character(20) :: encoding_mode1 = 'default'
  character(20) :: encoding_mode2 ='utf-16'

  !incorrect character expression in the ENCODING= specifier in open, statement

  open (UNIT=2, FILE='real4.dat', ENCODING=encoding_mode2) !incorrect specifier value

  open (UNIT=2, FILE='real4.dat', ENCODING=encoding_mode1)

close(2)

end program
