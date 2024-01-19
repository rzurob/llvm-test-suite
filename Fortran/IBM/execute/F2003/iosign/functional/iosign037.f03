!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE and OPEN
!*  stmts with format declarations for F edit descriptor and printing to a file
!*  with sequential access. Testing real variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign037

    real :: r = 1.0
    real(4) :: r4 = 1.0
    real(8) :: r8 = 1.0d0
    real(16) :: r16 = 1.0q0

    open (1,file="iosign037.1",form='formatted',access='sequential', &
          sign='plus')

    write (1,*) "\nTest 1\n"

10  format(A4,F5.2)
11  format(A4,F6.3)
12  format(A4,F7.4)

    write (1,10) "r=", r
    write (1,10,sign='processor_defined') "r4=", r4
    write (1,11) "r8=", r8
    write (1,12,sign='processor_defined') "r16=", r16

    write (1,10) "r=", r
    write (1,10,sign='plus') "r4=", r4
    write (1,11) "r8=", r8
    write (1,12,sign='plus') "r16=", r16

    write (1,10) "r=", r
    write (1,10,sign='suppress') "r4=", r4
    write (1,11) "r8=", r8
    write (1,12,sign='suppress') "r16=", r16

    close(1)

    open (1,file="iosign037.1",form='formatted',position='append',&
          access='sequential',sign='processor_defined')

    write (1,*) "\nTest 2\n"

13  format(A4,F5.2)
14  format(A4,F6.3)
15  format(A4,F7.4)

16  format(A4,SS,F5.2)
17  format(A4,SS,F6.3)
18  format(A4,SS,F7.4)

    write (1,13) "r=", r
    write (1,13,sign='processor_defined') "r4=", r4
    write (1,14) "r8=", r8
    write (1,15,sign='processor_defined') "r16=", r16

    write (1,16) "r=", r
    write (1,16,sign='plus') "r4=", r4
    write (1,17) "r8=", r8
    write (1,18,sign='plus') "r16=", r16

    write (1,13) "r=", r
    write (1,13,sign='suppress') "r4=", r4
    write (1,14) "r8=", r8
    write (1,15,sign='suppress') "r16=", r16

    close(1)

    open (1,file="iosign037.1",form='formatted',position='append',&
          access='sequential',sign='suppress')

    write (1,*) "\nTest 3\n"

19  format(A4,F5.2)
20  format(A4,F6.3)
21  format(A4,F7.4)

    write (1,19) "r=", r
    write (1,19,sign='processor_defined') "r4=", r4
    write (1,20) "r8=", r8
    write (1,21,sign='processor_defined') "r16=", r16

    write (1,19) "r=", r
    write (1,19,sign='plus') "r4=", r4
    write (1,20) "r8=", r8
    write (1,21,sign='plus') "r16=", r16

    write (1,19) "r=", r
    write (1,19,sign='suppress') "r4=", r4
    write (1,20) "r8=", r8
    write (1,21,sign='suppress') "r16=", r16

    close(1)

end program iosign037

