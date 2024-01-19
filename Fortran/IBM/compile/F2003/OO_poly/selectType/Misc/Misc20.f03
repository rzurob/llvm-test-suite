! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 26, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  ICE-Bad assumed length parameter
!*  (298869)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Misc20

! contains
! subroutine sub(C)

    CHARACTER(*), ALLOCATABLE :: C
    ALLOCATE(C, SOURCE="1")

!  end subroutine
  END



