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
!*  DESCRIPTION                :Test that file name and line no are displayed for
!*                              I/O failures at runtime
!*
!234567890123456789012345678901234567890123456789012345678901234567890
  integer :: i
  real :: j
  character(20) :: k
  namelist /ijk/ i,j,k

  call setrteopts('errloc=yes')

  open(2,file='iofilenamelineno001.nml')
  read(2,nml=ijk,decimal='point',round='up')

end

