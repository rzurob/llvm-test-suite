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
!*  format declarations and printing to a file with direct access, and testing 
!*  format SIGN selectors(SS,SP,S). Testing integer variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign013

    integer(2) :: i2 = 1
    integer(4) :: i4 = 1
    integer :: i = 1
    integer(8) :: i8 = 1
    integer :: nrec = 1

    open (1,file="iosign013.1",form='formatted',status='replace', &
          access='direct',recl=12)

    write (1,'(A10)',rec=nrec) "\nTest 1\n"

10  format(A4,I2)
11  format(A4,I4)
12  format(A4,I8)

    nrec = nrec + 1
    write (1,10,sign='processor_defined',rec=nrec) "i2=", i2
    nrec = nrec + 1
    write (1,11,sign='processor_defined',rec=nrec) "i4=", i4
    nrec = nrec + 1
    write (1,11,sign='processor_defined',rec=nrec) "i =", i
    nrec = nrec + 1
    write (1,12,sign='processor_defined',rec=nrec) "i8=", i8

    nrec = nrec + 1
    write (1,10,sign='plus',rec=nrec) "i2=", i2
    nrec = nrec + 1
    write (1,11,sign='plus',rec=nrec) "i4=", i4
    nrec = nrec + 1
    write (1,11,sign='plus',rec=nrec) "i =", i
    nrec = nrec + 1
    write (1,12,sign='plus',rec=nrec) "i8=", i8

    nrec = nrec + 1
    write (1,10,sign='suppress',rec=nrec) "i2=", i2
    nrec = nrec + 1
    write (1,11,sign='suppress',rec=nrec) "i4=", i4
    nrec = nrec + 1
    write (1,11,sign='suppress',rec=nrec) "i =", i
    nrec = nrec + 1
    write (1,12,sign='suppress',rec=nrec) "i8=", i8

    close(1)

    open (1,file="iosign013.1",form='formatted',status='old', &
           access='direct',recl=12)

    nrec = nrec + 1
    write (1,'(A10)',rec=nrec) "\nTest 2\n"

13  format(A4,SP,I2)
14  format(A4,SP,I4)
15  format(A4,SP,I8)

16  format(A4,SS,I2)
17  format(A4,SS,I4)
18  format(A4,SS,I8)

    nrec = nrec + 1
    write (1,13,sign='processor_defined',rec=nrec) "i2=", i2
    nrec = nrec + 1
    write (1,14,sign='processor_defined',rec=nrec) "i4=", i4
    nrec = nrec + 1
    write (1,14,sign='processor_defined',rec=nrec) "i =", i
    nrec = nrec + 1
    write (1,15,sign='processor_defined',rec=nrec) "i8=", i8

    nrec = nrec + 1
    write (1,16,sign='plus',rec=nrec) "i2=", i2
    nrec = nrec + 1
    write (1,17,sign='plus',rec=nrec) "i4=", i4
    nrec = nrec + 1
    write (1,17,sign='plus',rec=nrec) "i =", i
    nrec = nrec + 1
    write (1,18,sign='plus',rec=nrec) "i8=", i8

    nrec = nrec + 1
    write (1,13,sign='suppress',rec=nrec) "i2=", i2
    nrec = nrec + 1
    write (1,14,sign='suppress',rec=nrec) "i4=", i4
    nrec = nrec + 1
    write (1,14,sign='suppress',rec=nrec) "i =", i
    nrec = nrec + 1
    write (1,15,sign='suppress',rec=nrec) "i8=", i8

    close(1)

    open (1,file="iosign013.1",form='formatted',status='old',&
          access='direct',recl=12)

    nrec = nrec + 1
    write (1,'(A10)',rec=nrec) "\nTest 3\n"

19  format(A4,S,I2)
20  format(A4,S,I4)
21  format(A4,S,I8)

    nrec = nrec + 1
    write (1,19,sign='processor_defined',rec=nrec) "i2=", i2
    nrec = nrec + 1
    write (1,20,sign='processor_defined',rec=nrec) "i4=", i4
    nrec = nrec + 1
    write (1,20,sign='processor_defined',rec=nrec) "i =", i
    nrec = nrec + 1
    write (1,21,sign='processor_defined',rec=nrec) "i8=", i8

    nrec = nrec + 1
    write (1,19,sign='plus',rec=nrec) "i2=", i2
    nrec = nrec + 1
    write (1,20,sign='plus',rec=nrec) "i4=", i4
    nrec = nrec + 1
    write (1,20,sign='plus',rec=nrec) "i =", i
    nrec = nrec + 1
    write (1,21,sign='plus',rec=nrec) "i8=", i8

    nrec = nrec + 1
    write (1,19,sign='suppress',rec=nrec) "i2=", i2
    nrec = nrec + 1
    write (1,20,sign='suppress',rec=nrec) "i4=", i4
    nrec = nrec + 1
    write (1,20,sign='suppress',rec=nrec) "i =", i
    nrec = nrec + 1
    write (1,21,sign='suppress',rec=nrec) "i8=", i8

    close(1)

end program iosign013

