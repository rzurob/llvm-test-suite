!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 31, 2009
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
!*  Empty base type and array dummy
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj03

  TYPE base(l)
    INTEGER, LEN :: l
  END TYPE

  TYPE dt(l)
    INTEGER, LEN :: l
    TYPE(Base(l)) :: Arr(1:l)
    REAL          :: Arr1(l)=-1
  END TYPE

  CALL Sub(4)
  CONTAINS

  SUBROUTINE Sub(N)
  TYPE(dt(n)) b(n)

  IF (ubound(b(1)%arr,1)   .NE. 4)   STOP 11
  IF (SIZE(b(4)%arr)       .NE. 4)   STOP 12
  IF (ubound(b(1)%arr1,1)  .NE. 4)   STOP 13
  IF (SIZE(b(4)%arr1)      .NE. 4)   STOP 14
  IF (ANY(b(1)%arr1        .NE. -1)) STOP 15

  END SUBROUTINE

  END

