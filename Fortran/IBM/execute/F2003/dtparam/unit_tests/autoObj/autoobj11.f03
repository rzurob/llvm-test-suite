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
!*  The length parameter depends on a V in common block
!*
!*  ()
!*
!234567891123456789112345678911234567891123456789112345678911234567890

  PROGRAM autoobj11
  INTEGER :: N
  COMMON N
  N = 10
  CALL sub()
  END

  SUBROUTINE Sub()

  INTEGER :: N
  COMMON N

  TYPE dt(l)
     INTEGER, LEN :: l
     INTEGER      :: arr(l)=-1
  END TYPE

  TYPE(dt(N*2)) b

  PRINT*, size(b%arr)
  PRINT*, b%arr

  IF (b%l         .NE. 20)  ERROR STOP 11
  IF (SIZE(b%arr) .NE. 20)  ERROR STOP 12
  IF (ANY(b%arr   .NE. -1)) ERROR STOP 13

  END SUBROUTINE

