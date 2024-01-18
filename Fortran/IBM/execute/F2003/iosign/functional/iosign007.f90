!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Alberto Alvarez-Mesquida
!*  DATE                       : 02/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  free format and printing to the console. Testing complex variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign007

  complex :: q = (1.0,1.0)
  complex(4) :: q4 = (1.0,1.0)
  complex(8) :: q8 = (1.0d0,1.0d0)
  complex(16) :: q16 = (1.0q0,1.0q0)

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

end program iosign007

