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

program iosign004

    real :: r = 1.0
    real(4) :: r4 = 1.0
    real(8) :: r8 = 1.0d0
    real(16) :: r16 = 1.0q0

    print *, "\nTest 1\n"

10  format(A4,F5.2)
11  format(A4,F6.3)
12  format(A4,F7.4)

    write (*,10,sign='processor_defined') "r=", r
    write (*,10,sign='processor_defined') "r4=", r4
    write (*,11,sign='processor_defined') "r8=", r8
    write (*,12,sign='processor_defined') "r16=", r16

    write (*,10,sign='plus') "r=", r
    write (*,10,sign='plus') "r4=", r4
    write (*,11,sign='plus') "r8=", r8
    write (*,12,sign='plus') "r16=", r16

    write (*,10,sign='suppress') "r=", r
    write (*,10,sign='suppress') "r4=", r4
    write (*,11,sign='suppress') "r8=", r8
    write (*,12,sign='suppress') "r16=", r16

    print *, "\nTest 2\n"

13  format(A4,SP,F5.2)
14  format(A4,SP,F6.3)
15  format(A4,SP,F7.4)

16  format(A4,SS,F5.2)
17  format(A4,SS,F6.3)
18  format(A4,SS,F7.4)

    write (*,13,sign='processor_defined') "r=", r
    write (*,13,sign='processor_defined') "r4=", r4
    write (*,14,sign='processor_defined') "r8=", r8
    write (*,15,sign='processor_defined') "r16=", r16

    write (*,16,sign='plus') "r=", r
    write (*,16,sign='plus') "r4=", r4
    write (*,17,sign='plus') "r8=", r8
    write (*,18,sign='plus') "r16=", r16

    write (*,13,sign='suppress') "r=", r
    write (*,13,sign='suppress') "r4=", r4
    write (*,14,sign='suppress') "r8=", r8
    write (*,15,sign='suppress') "r16=", r16

    print *, "\nTest 3\n"

19  format(A4,S,F5.2)
20  format(A4,S,F6.3)
21  format(A4,S,F7.4)

    write (*,19,sign='processor_defined') "r=", r
    write (*,19,sign='processor_defined') "r4=", r4
    write (*,20,sign='processor_defined') "r8=", r8
    write (*,21,sign='processor_defined') "r16=", r16

    write (*,19,sign='plus') "r=", r
    write (*,19,sign='plus') "r4=", r4
    write (*,20,sign='plus') "r8=", r8
    write (*,21,sign='plus') "r16=", r16

    write (*,19,sign='suppress') "r=", r
    write (*,19,sign='suppress') "r4=", r4
    write (*,20,sign='suppress') "r8=", r8
    write (*,21,sign='suppress') "r16=", r16

end program iosign004
