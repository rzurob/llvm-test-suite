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
!*  Basic on default init on autoobj
!*  (RTO issue)
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj97

  CALL sub1(4)
  CALL sub(4)

! contains
  END

  SUBROUTINE Sub(n)

  TYPE dt(l)
    INTEGER, LEN :: l
    INTEGER :: arr(l)=-1
  END TYPE

  TYPE(dt(n)) b(n)

  ENTRY Sub1(n)


  IF (b%l            .NE. 4)  ERROR STOP 11
  IF (SIZE(b(1)%arr) .NE. 4)  ERROR STOP 12
  IF (SIZE(b)        .NE. 4)  ERROR STOP 13
  IF (ANY(b(1)%arr   .NE. -1 ))  ERROR STOP 14

  END SUBROUTINE


