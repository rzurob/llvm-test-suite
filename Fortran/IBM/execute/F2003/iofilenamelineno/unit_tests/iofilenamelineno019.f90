!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: iofilenamelineno019.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 1, 2019
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
  character(10) :: k
  character(20) :: format

  namelist /nml_1/ i,j,k

  call setrteopts('errloc=no')

  open(2,file='iofilenamelineno019.nml')

  read(2,nml=nml_1, round='processor_defined', decimal='point', blank='null', pad='yes')

  format='unformatted'

  open(unit=2,file='iofilenamelineno002.dat', form=format)

  read(2,fmt='(i8)',decimal='point', round='down', blank='null') i

end
