!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  free format and printing to a file with sequential access. Testing integer
!*  variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign011

  integer(2) :: i2 = 1
  integer :: i = 1
  integer(4) :: i4 = 1
  integer(8) :: i8 = 1

  open (1,file="iosign011.1",form='formatted',access='sequential')

  write (1,*,sign='processor_defined') "i2=", i2
  write (1,*,sign='processor_defined') "i4=", i4
  write (1,*,sign='processor_defined') "i =", i
  write (1,*,sign='processor_defined') "i8=", i8

  write (1,*,sign='plus') "i2=", i2
  write (1,*,sign='plus') "i4=", i4
  write (1,*,sign='plus') "i =", i
  write (1,*,sign='plus') "i8=", i8

  write (1,*,sign='suppress') "i2=", i2
  write (1,*,sign='suppress') "i4=", i4
  write (1,*,sign='suppress') "i =", i
  write (1,*,sign='suppress') "i8=", i8

  close(1)

end program iosign011

