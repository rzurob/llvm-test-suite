!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier005.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE TITLE            : roundspecifier005
!*
!*  PROGRAMMER                 : Michael Selvanayagam
!*  DATE                       : Dec. 20, 2005
!*  ORIGIN                     : AIX Compiler Development,
!*                             : IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  DRIVER STANZA              : xlf95
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  DESCRIPTION                : diagnostic testing of incorrect use of
!*                               ROUND= specifier in I/O statements at 
!*                               compile time
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num, round_mode_num
   
  character(20) :: round_mode = 'down'
  
  !only character expressions are allowed for the ROUND= specifier in open, read and write statements
  
  open (UNIT=2, FILE='roundspecifier.dat', ROUND=round_mode_num)
  
  open (UNIT=2, FILE='roundspecifier.dat', ROUND=round_mode)
  
  read (UNIT=2,FMT='(f7.4)', ROUND=round_mode_num) num
  
  write (UNIT=2, FMT='(f7.4)', ROUND=round_mode_num) num
  
  !only character variables are allowed for the ROUND= specifer in inquire statements
  
  inquire(2, round=round_mode_num)
  
  close(2)

end program