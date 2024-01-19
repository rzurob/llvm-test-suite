!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  format declarations for F edit descriptor, and testing format SIGN selectors
!*  (SS,SP,S), printing to the console. Using complex variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign098

    complex :: q = (1.0,1.0)
    complex(4) :: q4 = (1.0,1.0)
    complex(8) :: q8 = (1.0d0,1.0d0)
    complex(16) :: q16 = (1.0q0,1.0q0)

    print *, "\nTest 1\n"

13  format(A4,X,SP,F5.2,X,SS,F5.2)
14  format(A4,X,SP,F6.3,X,SS,F6.3)
15  format(A4,X,SP,F7.4,X,SS,F7.4)

    write (*,13) "q=", q
    write (*,13) "q4=", q4
    write (*,14) "q8=", q8
    write (*,15) "q16=", q16

end program iosign098

