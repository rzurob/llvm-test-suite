!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iofilenamelineno011.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 1, 2006
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : Display file name and line no in I/O failures
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :Test that file name and line no are displayed for
!*                              I/O failures at runtime
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  integer :: i
  real :: j
  character(20) :: k
  namelist /ijk/ i,j,k

  call setrteopts('errloc=no')

  open(2,file='iofilenamelineno011.nml')
  read(2,nml=ijk,decimal='point',round='up')

end

