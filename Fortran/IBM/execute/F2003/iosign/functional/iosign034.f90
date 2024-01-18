!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE and OPEN
!*  stmts with format declarations and printing to a file with sequential access
!*  , and testing format SIGN selectors(SS,SP,S). Testing integer variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign034

    integer(2) :: i2 = 1
    integer(4) :: i4 = 1
    integer :: i = 1
    integer(8) :: i8 = 1

    open (1,file="iosign034.1",form='formatted',access='sequential',&
          sign='plus')

    write (1,*) "\nTest 1\n"

10  format(A4,I2)
11  format(A4,I4)
12  format(A4,I8)

    write (1,10) "i2=", i2
    write (1,11,sign='processor_defined') "i4=", i4
    write (1,11) "i =", i
    write (1,12,sign='processor_defined') "i8=", i8

    write (1,10) "i2=", i2
    write (1,11,sign='plus') "i4=", i4
    write (1,11) "i =", i
    write (1,12,sign='plus') "i8=", i8

    write (1,10) "i2=", i2
    write (1,11,sign='suppress') "i4=", i4
    write (1,11) "i =", i
    write (1,12,sign='suppress') "i8=", i8

    close(1)

    open (1,file="iosign034.1",form='formatted',position='append',&
          access='sequential',sign='plus')

    write (1,*) "\nTest 2\n"

13  format(A4,SP,I2)
14  format(A4,SP,I4)
15  format(A4,SP,I8)

16  format(A4,SS,I2)
17  format(A4,SS,I4)
18  format(A4,SS,I8)

    write (1,13) "i2=", i2
    write (1,14,sign='processor_defined') "i4=", i4
    write (1,14) "i =", i
    write (1,15,sign='processor_defined') "i8=", i8

    write (1,16) "i2=", i2
    write (1,17,sign='plus') "i4=", i4
    write (1,17) "i =", i
    write (1,18,sign='plus') "i8=", i8

    write (1,13) "i2=", i2
    write (1,14,sign='suppress') "i4=", i4
    write (1,14) "i =", i
    write (1,15,sign='suppress') "i8=", i8

    close(1)

    open (1,file="iosign034.1",form='formatted',position='append',&
          access='sequential',sign='plus')

    write (1,*) "\nTest 3\n"

19  format(A4,S,I2)
20  format(A4,S,I4)
21  format(A4,S,I8)

    write (1,19) "i2=", i2
    write (1,20,sign='processor_defined') "i4=", i4
    write (1,20) "i =", i
    write (1,21,sign='processor_defined') "i8=", i8

    write (1,19) "i2=", i2
    write (1,20,sign='plus') "i4=", i4
    write (1,20) "i =", i
    write (1,21,sign='plus') "i8=", i8

    write (1,19) "i2=", i2
    write (1,20,sign='suppress') "i4=", i4
    write (1,20) "i =", i
    write (1,21,sign='suppress') "i8=", i8

    close(1)

end program iosign034

