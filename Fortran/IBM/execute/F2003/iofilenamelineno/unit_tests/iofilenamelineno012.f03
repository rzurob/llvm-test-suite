!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 1, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Display file name and line no in I/O failures
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : Test that file name and line no are displayed for
!*                              I/O failures at runtime
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  integer :: i
  character(20) :: format

  call setrteopts('errloc=no')

  format='unformatted'

  open(unit=2,file='iofilenamelineno012.dat', form=format)
  read(2,fmt='(i8)',decimal='point', round='down', blank='null', end=100) i


100 end
