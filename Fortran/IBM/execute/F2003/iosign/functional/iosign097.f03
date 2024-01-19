!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  format declarations for F edit descriptor, and testing format SIGN selectors
!*  (SS,SP,S), printing to the console. Using real variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign097

    integer, parameter :: n = 3
    real :: r(n) = 1.0
    real(4) :: r4(n) = 1.0
    real(8) :: r8(n) = 1.0d0
    real(16) :: r16(n) = 1.0q0

    print *, "\nTest 1\n"

10  format(A4,X,S,F5.2,X,SP,F5.2,X,SS,F5.2)
11  format(A4,X,S,F6.3,X,SP,F6.3,X,SS,F6.3)
12  format(A4,X,S,F7.4,X,SP,F7.4,X,SS,F7.4)

    write (*,10) "r=", r
    write (*,10) "r4=", r4
    write (*,11) "r8=", r8
    write (*,12) "r16=", r16

end program iosign097

