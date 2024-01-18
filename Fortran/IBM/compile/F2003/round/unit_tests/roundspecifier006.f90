!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier006.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of incorrect use of
!*                               ROUND= specifier in I/O statements at
!*                               compile time
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num

  character(20) :: round_mode = 'down'

  !incorrect character expression in the ROUND= specifier in open, read and write statements

  open (UNIT=2, FILE='real4.dat', ROUND='updown') !incorrect specifier value

  open (UNIT=2, FILE='real4.dat', ROUND=round_mode)

  read (UNIT=2,FMT='(f7.4)', ROUND='processor') num !incorrect specifier value

  write (UNIT=2, FMT='(f7.4)', ROUND='compat') num    !incorrect specifier value

  wait(2,ROUND='up')

  backspace(2,ROUND='nearest')

  flush(2,ROUND='down')

  close(2,ROUND='down')


end program