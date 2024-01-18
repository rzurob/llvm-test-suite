!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : April 30, 2007
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ENCODING= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=95std
!*
!*  DESCRIPTION                : diagnostic testing of the ENCODING= specifier
!*                               in I/O statements at compile time with -qlanglvl
!*                               set to a non F2003 level
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num
  character(20) :: encoding_mode

  !ENCODING= specifier not allowed when -qlanglvl=95std

  open(UNIT=2,FILE='encodingspecifier.dat', ENCODING='default')

  inquire(2,ENCODING=encoding_mode)

  close(2)

end program
