!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  free format and printing to the console. Testing integer variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign000

  integer(2) :: i2 = 1
  integer(4) :: i4 = 1
  integer :: i = 1
  integer(8) :: i8 = 1

  write (*,*,sign='processor_defined') "i2=", i2
  write (*,*,sign='processor_defined') "i4=", i4
  write (*,*,sign='processor_defined') "i =", i
  write (*,*,sign='processor_defined') "i8=", i8

  write (*,*,sign='plus') "i2=", i2
  write (*,*,sign='plus') "i4=", i4
  write (*,*,sign='plus') "i =", i
  write (*,*,sign='plus') "i8=", i8

  write (*,*,sign='suppress') "i2=", i2
  write (*,*,sign='suppress') "i4=", i4
  write (*,*,sign='suppress') "i =", i
  write (*,*,sign='suppress') "i8=", i8

end program iosign000

