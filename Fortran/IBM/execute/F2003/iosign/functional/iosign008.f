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

program iosign008

    complex :: q = (1.0,1.0)
    complex(4) :: q4 = (1.0,1.0)
    complex(8) :: q8 = (1.0d0,1.0d0)
    complex(16) :: q16 = (1.0q0,1.0q0)

    print *, "\nTest 1\n"

10  format(A4,2(X,F5.2))
11  format(A4,2(X,F6.3))
12  format(A4,2(X,F7.4))

    write (*,10,sign='processor_defined') "q=", q
    write (*,10,sign='processor_defined') "q4=", q4
    write (*,11,sign='processor_defined') "q8=", q8
    write (*,12,sign='processor_defined') "q16=", q16

    write (*,10,sign='plus') "q=", q
    write (*,10,sign='plus') "q4=", q4
    write (*,11,sign='plus') "q8=", q8
    write (*,12,sign='plus') "q16=", q16

    write (*,10,sign='suppress') "q=", q
    write (*,10,sign='suppress') "q4=", q4
    write (*,11,sign='suppress') "q8=", q8
    write (*,12,sign='suppress') "q16=", q16

    print *, "\nTest 2\n"

13  format(A4,SP,2(X,F5.2))
14  format(A4,SP,2(X,F6.3))
15  format(A4,SP,2(X,F7.4))

16  format(A4,SS,2(X,F5.2))
17  format(A4,SS,2(X,F6.3))
18  format(A4,SS,2(X,F7.4))

    write (*,13,sign='processor_defined') "q=", q
    write (*,13,sign='processor_defined') "q4=", q4
    write (*,14,sign='processor_defined') "q8=", q8
    write (*,15,sign='processor_defined') "q16=", q16

    write (*,16,sign='plus') "q=", q
    write (*,16,sign='plus') "q4=", q4
    write (*,17,sign='plus') "q8=", q8
    write (*,18,sign='plus') "q16=", q16

    write (*,13,sign='suppress') "q=", q
    write (*,13,sign='suppress') "q4=", q4
    write (*,14,sign='suppress') "q8=", q8
    write (*,15,sign='suppress') "q16=", q16

    print *, "\nTest 3\n"

19  format(A4,S,2(X,F5.2))
20  format(A4,S,2(X,F6.3))
21  format(A4,S,2(X,F7.4))

    write (*,19,sign='processor_defined') "q=", q
    write (*,19,sign='processor_defined') "q4=", q4
    write (*,20,sign='processor_defined') "q8=", q8
    write (*,21,sign='processor_defined') "q16=", q16

    write (*,19,sign='plus') "q=", q
    write (*,19,sign='plus') "q4=", q4
    write (*,20,sign='plus') "q8=", q8
    write (*,21,sign='plus') "q16=", q16

    write (*,19,sign='suppress') "q=", q
    write (*,19,sign='suppress') "q4=", q4
    write (*,20,sign='suppress') "q8=", q8
    write (*,21,sign='suppress') "q16=", q16

end program iosign008

