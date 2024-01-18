!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 25, 2008
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
!*  The length parameter depends on dummy
!*  the array bound relies on dummy
!*
!*  ()
!*
!234567890423456789022345678902234567890223456789022345678902234567890

  PROGRAM autoobj04
  CALL sub(2)
  contains

  SUBROUTINE Sub(N)

  TYPE base(l)
    INTEGER, LEN :: l
    CHARACTER(l) :: c!="123"
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(base(l)) :: arr(l)
  END TYPE

  TYPE(dt(n)) b(n)

  IF (b%l               .NE. 2)  STOP 11
  IF (b(1)%arr%l        .NE. 2)  STOP 12
  IF (len(b(1)%arr%c)   .NE. 2)  STOP 13
  IF (SIZE(b(1)%arr)    .NE. 2)  STOP 14
  IF (SIZE(b)           .NE. 2)  STOP 15

  b(1)%arr%c = '123'
print*,  b(1)%arr%c
  IF (ANY(b(1)%arr%c  .NE. "12")) STOP 16

  END SUBROUTINE
  END

