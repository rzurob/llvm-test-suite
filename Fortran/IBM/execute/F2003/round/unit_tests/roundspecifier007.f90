!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier007.f
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
!*                               run time
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num, round_mode_num

  character(20) :: round_mode1 = 'down'
  character(20) :: round_mode2 ='updown'

  !incorrect character expression in the ROUND= specifier in open, read and write statements

  open (UNIT=2, FILE='real4.dat', ROUND=round_mode2) !incorrect specifier value

  open (UNIT=2, FILE='real4.dat', ROUND=round_mode1)

  read (UNIT=2,FMT='(f7.4)', ROUND=round_mode2) num !incorrect specifier value

  write (UNIT=2, FMT='(f7.4)', ROUND=round_mode2) num    !incorrect specifier value

close(2)

end program