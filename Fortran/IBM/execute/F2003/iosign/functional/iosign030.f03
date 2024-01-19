!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  format declarations for G edit descriptor and printing to a file with
!*  sequential access, and testing format SIGN selectors(SS,SP,S). Testing
!*  complex variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign030

    complex :: q = (1.0,1.0)
    complex(4) :: q4 = (1.0,1.0)
    complex(8) :: q8 = (1.0d0,1.0d0)
    complex(16) :: q16 = (1.0q0,1.0q0)

    open (1,file="iosign030.1",form='formatted',access='sequential')

    write (1,*) "\nTest 1\n"

10  format(A4,2(X,G10.4))
11  format(A4,2(X,G12.5))
12  format(A4,2(X,G14.6))

    write (1,10,sign='processor_defined') "q=", q
    write (1,10,sign='processor_defined') "q4=", q4
    write (1,11,sign='processor_defined') "q8=", q8
    write (1,12,sign='processor_defined') "q16=", q16

    write (1,10,sign='plus') "q=", q
    write (1,10,sign='plus') "q4=", q4
    write (1,11,sign='plus') "q8=", q8
    write (1,12,sign='plus') "q16=", q16

    write (1,10,sign='suppress') "q=", q
    write (1,10,sign='suppress') "q4=", q4
    write (1,11,sign='suppress') "q8=", q8
    write (1,12,sign='suppress') "q16=", q16

    close(1)

    open (1,file="iosign030.1",form='formatted',position='append',&
          access='sequential')

    write (1,*) "\nTest 2\n"

13  format(A4,SP,2(X,G10.4))
14  format(A4,SP,2(X,G12.5))
15  format(A4,SP,2(X,G14.6))

16  format(A4,SS,2(X,G10.4))
17  format(A4,SS,2(X,G12.5))
18  format(A4,SS,2(X,G14.6))

    write (1,13,sign='processor_defined') "q=", q
    write (1,13,sign='processor_defined') "q4=", q4
    write (1,14,sign='processor_defined') "q8=", q8
    write (1,15,sign='processor_defined') "q16=", q16

    write (1,16,sign='plus') "q=", q
    write (1,16,sign='plus') "q4=", q4
    write (1,17,sign='plus') "q8=", q8
    write (1,18,sign='plus') "q16=", q16

    write (1,13,sign='suppress') "q=", q
    write (1,13,sign='suppress') "q4=", q4
    write (1,14,sign='suppress') "q8=", q8
    write (1,15,sign='suppress') "q16=", q16

    close(1)

    open (1,file="iosign030.1",form='formatted',position='append',&
          access='sequential')

    write (1,*) "\nTest 3\n"

19  format(A4,S,2(X,G10.4))
20  format(A4,S,2(X,G12.5))
21  format(A4,S,2(X,G14.6))

    write (1,19,sign='processor_defined') "q=", q
    write (1,19,sign='processor_defined') "q4=", q4
    write (1,20,sign='processor_defined') "q8=", q8
    write (1,21,sign='processor_defined') "q16=", q16

    write (1,19,sign='plus') "q=", q
    write (1,19,sign='plus') "q4=", q4
    write (1,20,sign='plus') "q8=", q8
    write (1,21,sign='plus') "q16=", q16

    write (1,19,sign='suppress') "q=", q
    write (1,19,sign='suppress') "q4=", q4
    write (1,20,sign='suppress') "q8=", q8
    write (1,21,sign='suppress') "q16=", q16

    close(1)

end program iosign030

