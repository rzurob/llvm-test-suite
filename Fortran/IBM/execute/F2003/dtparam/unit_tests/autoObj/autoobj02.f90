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
!*  the component is character
!*
!*  ()
!*
!234567890223456789022345678902234567890223456789022345678902234567890

  PROGRAM autoobj02
  CALL sub(2)
  END

  SUBROUTINE Sub(N)

  integer n

  TYPE dt(l)
    INTEGER, LEN :: l
    CHARACTER(l) :: arr(l)="12345"
  END TYPE

  TYPE(dt(n*2)) b

  PRINT*, size(b%arr)
  PRINT*, b%arr

  IF (b%l         .NE. 4)  STOP 11
  IF (LEN(b%arr)  .NE. 4)  STOP 12
  IF (SIZE(b%arr) .NE. 4)  STOP 13

  ! The .NE. seems wrong! -> 338494
  IF (ANY(b%arr   .NE. "1234")) STOP 14

  END SUBROUTINE

