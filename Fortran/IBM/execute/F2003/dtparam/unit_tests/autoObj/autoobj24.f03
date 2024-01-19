!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 30, 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DTPARAM: Automatic objects
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 333321
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The length parameter depends on V in host
!*  the array bound relies on v
!*
!*  ()
!*
!234567892423456789022345678902234567890223456789022345678902234567890

  PROGRAM autoobj24
  INTEGER N

  N = 2
  CALL sub()

  CONTAINS

  SUBROUTINE Sub()

  TYPE base(l)
    INTEGER, LEN :: l
    CHARACTER(l) :: c="123"
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr(l)
  END TYPE

  TYPE(dt(n)) b(n)

print*, b%l
print*, SIZE(b(1)%arr)
print*, len(b(1)%arr%c)

  IF (b%l            .NE. 2)  ERROR STOP 11
  IF (b(1)%arr%l        .NE. 2)  ERROR STOP 12
  IF (len(b(1)%arr%c)    .NE. 2)  ERROR STOP 13
  IF (SIZE(b(1)%arr) .NE. 2)  ERROR STOP 14
  IF (SIZE(b)        .NE. 2)  ERROR STOP 15

  do i = 1, n
        b(i)%arr%c = '123'
  end do

  IF (ANY(b(1)%arr%c  .NE. "12")) ERROR STOP 16
  IF (ANY(b(n)%arr%c  .NE. "12")) ERROR STOP 17

  END SUBROUTINE
  END

