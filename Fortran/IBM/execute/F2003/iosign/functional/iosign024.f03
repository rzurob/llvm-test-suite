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
!*  real variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign024

    real :: r = 1.0
    real(4) :: r4 = 1.0
    real(8) :: r8 = 1.0d0
    real(16) :: r16 = 1.0q0

    open (1,file="iosign024.1",form='formatted',access='sequential')

    write (1,*) "\nTest 1\n"

10  format(A4,G10.4)
11  format(A4,G12.5)
12  format(A4,G14.6)

    write (1,10,sign='processor_defined') "r=", r
    write (1,10,sign='processor_defined') "r4=", r4
    write (1,11,sign='processor_defined') "r8=", r8
    write (1,12,sign='processor_defined') "r16=", r16

    write (1,10,sign='plus') "r=", r
    write (1,10,sign='plus') "r4=", r4
    write (1,11,sign='plus') "r8=", r8
    write (1,12,sign='plus') "r16=", r16

    write (1,10,sign='suppress') "r=", r
    write (1,10,sign='suppress') "r4=", r4
    write (1,11,sign='suppress') "r8=", r8
    write (1,12,sign='suppress') "r16=", r16

    close(1)

    open (1,file="iosign024.1",form='formatted',position='append',&
          access='sequential')

    write (1,*) "\nTest 2\n"

13  format(A4,SP,G10.4)
14  format(A4,SP,G12.5)
15  format(A4,SP,G14.6)

16  format(A4,SS,G10.4)
17  format(A4,SS,G12.5)
18  format(A4,SS,G14.6)

    write (1,13,sign='processor_defined') "r=", r
    write (1,13,sign='processor_defined') "r4=", r4
    write (1,14,sign='processor_defined') "r8=", r8
    write (1,15,sign='processor_defined') "r16=", r16

    write (1,16,sign='plus') "r=", r
    write (1,16,sign='plus') "r4=", r4
    write (1,17,sign='plus') "r8=", r8
    write (1,18,sign='plus') "r16=", r16

    write (1,13,sign='suppress') "r=", r
    write (1,13,sign='suppress') "r4=", r4
    write (1,14,sign='suppress') "r8=", r8
    write (1,15,sign='suppress') "r16=", r16

    close(1)

    open (1,file="iosign024.1",form='formatted',position='append',&
          access='sequential')

    write (1,*) "\nTest 3\n"

19  format(A4,S,G10.4)
20  format(A4,S,G12.5)
21  format(A4,S,G14.6)

    write (1,19,sign='processor_defined') "r=", r
    write (1,19,sign='processor_defined') "r4=", r4
    write (1,20,sign='processor_defined') "r8=", r8
    write (1,21,sign='processor_defined') "r16=", r16

    write (1,19,sign='plus') "r=", r
    write (1,19,sign='plus') "r4=", r4
    write (1,20,sign='plus') "r8=", r8
    write (1,21,sign='plus') "r16=", r16

    write (1,19,sign='suppress') "r=", r
    write (1,19,sign='suppress') "r4=", r4
    write (1,20,sign='suppress') "r8=", r8
    write (1,21,sign='suppress') "r16=", r16

    close(1)

end program iosign024

