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
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM autoobj01
  CALL sub(10)
  END

  SUBROUTINE Sub(N)

  integer n

  TYPE dt(l)
     INTEGER, LEN :: l
     INTEGER      :: arr(l)=-1
  END TYPE

  TYPE(dt(n*2)) b

  PRINT*, size(b%arr)
  PRINT*, b%arr

  IF (b%l         .NE. 20)  STOP 11
  IF (SIZE(b%arr) .NE. 20)  STOP 12
  IF (ANY(b%arr   .NE. -1)) STOP 13

  END SUBROUTINE

