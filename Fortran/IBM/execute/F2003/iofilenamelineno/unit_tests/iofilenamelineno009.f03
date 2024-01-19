!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : March 1, 2009
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

  call setrteopts("errloc=yes")

  open(2,file='iofilenamelineno009.nml')

  read(2,nml=nml_1, round='processor_defined', decimal='point', blank='null', pad='yes')

  format='unformatted'

  open(unit=2,file='iofilenamelineno002.dat', form=format)

  read(2,fmt='(i8)',decimal='point', round='down', blank='null') i

end
