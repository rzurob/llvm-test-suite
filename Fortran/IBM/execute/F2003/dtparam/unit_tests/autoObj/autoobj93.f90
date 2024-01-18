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
!*  spec on allocatable array -- a real autoobj?
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM autoobj93

  TYPE DT(l)
    INTEGER, LEN :: l
    INTEGER :: arr(l)=-1
  END TYPE

   CALL sub(4)
  contains
  SUBROUTINE Sub(n)

  TYPE(dt(N)), allocatable :: t(:)

    ALLOCATE(t(N))

    IF (t%l                 .NE. 4)     STOP 11
    IF (UBOUND(t(N)%arr, 1) .NE. 4)     STOP 12
    IF (SIZE(t(N)%arr)      .NE. 4)     STOP 13
    IF (ANY(t(N)%arr        .NE. -1))   STOP 14

  END SUBROUTINE

  END

