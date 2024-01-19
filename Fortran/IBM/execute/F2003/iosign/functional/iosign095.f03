!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  free format and printing to the console zero values. Testing real variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign095

  real :: r = -0.0
  real(4) :: r4 = -0.0
  real(8) :: r8 = -0.0d0
  real(16) :: r16 = -0.0q0

  write (*,*,sign='processor_defined') "r =", r
  write (*,*,sign='processor_defined') "r4=", r4
  write (*,*,sign='processor_defined') "r8=", r8
  write (*,*,sign='processor_defined') "r16=", r16

  write (*,*,sign='plus') "r =", r
  write (*,*,sign='plus') "r4=", r4
  write (*,*,sign='plus') "r8=", r8
  write (*,*,sign='plus') "r16=", r16

  write (*,*,sign='suppress') "r =", r
  write (*,*,sign='suppress') "r4=", r4
  write (*,*,sign='suppress') "r8=", r8
  write (*,*,sign='suppress') "r16=", r16

end program iosign095

