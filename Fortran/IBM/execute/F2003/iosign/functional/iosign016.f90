!*  ===================================================================
!*
!*  DATE                       : 02/20/2006
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing SIGN specifier with the WRITE stmt with
!*  format declarations for F edit descriptor and printing to a file with
!*  direct access, and testing format SIGN selectors(SS,SP,S). Testing real
!*  variables.
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program iosign016

    real :: r = 1.0
    real(4) :: r4 = 1.0
    real(8) :: r8 = 1.0d0
    real(16) :: r16 = 1.0q0

    integer :: nrec = 1

    open (1,file="iosign016.1",form='formatted',status='replace', &
          access='direct',recl=12)

    nrec = nrec + 1
    write (1,'(A10)',rec=nrec) "\nTest 1\n"

10  format(A4,F5.2)
11  format(A4,F6.3)
12  format(A4,F7.4)

    nrec = nrec + 1
    write (1,10,sign='processor_defined',rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,10,sign='processor_defined',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,11,sign='processor_defined',rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,12,sign='processor_defined',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,10,sign='plus',rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,10,sign='plus',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,11,sign='plus',rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,12,sign='plus',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,10,sign='suppress',rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,10,sign='suppress',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,11,sign='suppress',rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,12,sign='suppress',rec=nrec) "r16=", r16

    close(1)

    open (1,file="iosign016.1",form='formatted',status='old', &
           access='direct',recl=12)

    nrec = nrec + 1
    write (1,'(A10)',rec=nrec) "\nTest 2\n"

13  format(A4,SP,F5.2)
14  format(A4,SP,F6.3)
15  format(A4,SP,F7.4)

16  format(A4,SS,F5.2)
17  format(A4,SS,F6.3)
18  format(A4,SS,F7.4)

    nrec = nrec + 1
    write (1,13,sign='processor_defined',rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,13,sign='processor_defined',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,14,sign='processor_defined',rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,15,sign='processor_defined',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,16,sign='plus',rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,16,sign='plus',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,17,sign='plus',rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,18,sign='plus',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,13,sign='suppress',rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,13,sign='suppress',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,14,sign='suppress',rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,15,sign='suppress',rec=nrec) "r16=", r16

    close(1)

    open (1,file="iosign016.1",form='formatted',status='old',&
          access='direct',recl=12)

    nrec = nrec + 1
    write (1,'(A10)',rec=nrec) "\nTest 3\n"

19  format(A4,S,F5.2)
20  format(A4,S,F6.3)
21  format(A4,S,F7.4)

    nrec = nrec + 1
    write (1,19,sign='processor_defined',rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,19,sign='processor_defined',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,20,sign='processor_defined',rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,21,sign='processor_defined',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,19,sign='plus',rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,19,sign='plus',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,20,sign='plus',rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,21,sign='plus',rec=nrec) "r16=", r16

    nrec = nrec + 1
    write (1,19,sign='suppress',rec=nrec) "r=", r
    nrec = nrec + 1
    write (1,19,sign='suppress',rec=nrec) "r4=", r4
    nrec = nrec + 1
    write (1,20,sign='suppress',rec=nrec) "r8=", r8
    nrec = nrec + 1
    write (1,21,sign='suppress',rec=nrec) "r16=", r16

    close(1)

end program iosign016

