!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: roundspecifier001.f
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
!*  TEST CASE TITLE            : roundspecifier001
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
!*  REQUIRED COMPILER OPTIONS  : -qlanglvl=95std
!*
!*  DESCRIPTION                : diagnostic testing of the ROUND= specifier
!*                               in I/O statements at compile time with -qlanglvl
!*                               set to a non F2003 level
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num
  character(20) :: round_mode

  !ROUND= specifier not allowed when -qlanglvl=95std
  
  open(UNIT=2,FILE='roundspecifier.dat', ROUND='zero')
  
  read(UNIT=2, FMT='(f7.4)',ROUND='down') num
  
  write(2,FMT='(f7.4)',ROUND='nearest') num
  
  read(UNIT=2, FMT='(f7.4)',ROUND='up') num

  write(2,FMT='(f7.4)',ROUND='zero') num

  read(UNIT=2, FMT='(f7.4)',ROUND='compatible') num

  write(2,FMT='(f7.4)',ROUND='processor_defined') num

  inquire(2,ROUND=round_mode)

  close(2)

  !round edit descriptors not allowed when -qlanglvl=95std

  open(UNIT=2,FILE='roundspecifier.dat', ROUND='zero')

  read(2,'(f7.4,ru)') num

  write(2, '(f7.4,rd)') num

  print('(f7.4,rn)'), num

  write(2,'(f7.4,rz)') num
 
  read(2,'(f7.4,rc)') num

  print('(f7.4,rp)'), num



  close(2)

end program



 
