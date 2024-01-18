!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 20, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : ROUND= specifier in I/O statements
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                : diagnostic testing of the ROUND= specifier
!*                               in I/O statements at compile time with
!*                               unformatted I/O
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  real :: num

  !ROUND= specifier not allowed in unformatted I/O

  open(UNIT=2,FILE='roundspecifier.dat', FORM='unformatted', ROUND='up')

  read(UNIT=3, FMT='(f7.4)',ROUND='compatible') num

  write(3,FMT='(f7.4)',ROUND='processor_defined') num

  close(2)

end program