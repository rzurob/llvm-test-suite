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
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE and OPEN
!*  stmt with format declarations for G edit descriptor and printing to a file 
!*  with direct access. Testing real variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign047

    real :: r = 1.0
    real(4) :: r4 = 1.0
    real(8) :: r8 = 1.0d0
    real(16) :: r16 = 1.0q0

    integer :: nrec = 1

    open (1,file="iosign047.1",form='formatted',status='replace', &
          access='direct',sign='processor_defined',recl=20)

    nrec = nrec + 1
    write (1,'(A10)',rec=nrec) "\nTest 1\n"

10  format(A4,G10.4)
11  format(A4,G12.5)
12  format(A4,G14.6)

    nrec = nrec + 1
    write (1,10,rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,10,sign='processor_defined',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,11,rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,12,sign='processor_defined',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,10,rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,10,sign='plus',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,11,rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,12,sign='plus',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,10,rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,10,sign='suppress',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,11,rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,12,sign='suppress',rec=nrec) "r16=", r16

    close(1)

    open (1,file="iosign047.1",form='formatted',status='old', &
           access='direct',sign='suppress',recl=20)

    nrec = nrec + 1
    write (1,'(A10)',rec=nrec) "\nTest 2\n"

13  format(A4,G10.4)
14  format(A4,G12.5)
15  format(A4,G14.6)

16  format(A4,G10.4)
17  format(A4,G12.5)
18  format(A4,G14.6)

    nrec = nrec + 1
    write (1,13,rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,13,sign='processor_defined',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,14,rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,15,sign='processor_defined',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,16,rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,16,sign='plus',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,17,rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,18,sign='plus',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,13,rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,13,sign='suppress',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,14,rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,15,sign='suppress',rec=nrec) "r16=", r16

    close(1)

    open (1,file="iosign047.1",form='formatted',status='old',&
          access='direct',sign='plus',recl=20)

    nrec = nrec + 1
    write (1,'(A10)',rec=nrec) "\nTest 3\n"

19  format(A4,G10.4)
20  format(A4,G12.5)
21  format(A4,G14.6)

    nrec = nrec + 1
    write (1,19,rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,19,sign='processor_defined',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,20,rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,21,sign='processor_defined',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,19,rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,19,sign='plus',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,20,rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,21,sign='plus',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,19,rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,19,sign='suppress',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,20,rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,21,sign='suppress',rec=nrec) "r16=", r16

    close(1)

end program iosign047

