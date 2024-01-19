!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  free format and printing to the console zero values. Testing complex
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

program iosign096

  complex :: q = (-0.0,0.0)
  complex(4) :: q4 = (0.0,-0.0)
  complex(8) :: q8 = (-0.0d0,0.0d0)
  complex(16) :: q16 = (0.0q0,-0.0q0)

  write (*,*,sign='processor_defined') "q =", q
  write (*,*,sign='processor_defined') "q4=", q4
  write (*,*,sign='processor_defined') "q8=", q8
  write (*,*,sign='processor_defined') "q16=", q16

  write (*,*,sign='plus') "q =", q
  write (*,*,sign='plus') "q4=", q4
  write (*,*,sign='plus') "q8=", q8
  write (*,*,sign='plus') "q16=", q16

  write (*,*,sign='suppress') "q =", q
  write (*,*,sign='suppress') "q4=", q4
  write (*,*,sign='suppress') "q8=", q8
  write (*,*,sign='suppress') "q16=", q16

end program iosign096

