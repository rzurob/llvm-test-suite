! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 18, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Structure component - initialization-expr
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
!     SEQUENCE  !?
      INTEGER :: BaseID=1
      PROCEDURE(LOGICAL(2)), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE  :: DT
!     SEQUENCE
      INTEGER :: ChildID=2
      TYPE(Base) :: BComp=Base(-1, NULL())
    END TYPE

  END MODULE


  PROGRAM StrComp3
  USE M
  IMPLICIT NONE

  TYPE(DT) :: U
  TYPE(DT) :: V=DT(-2,     &
                & Base(-1, NULL()))


  IF ( V%ChildId .NE. -2 ) ERROR STOP 12
  IF ( V%BComp%BaseId .NE. -1 ) ERROR STOP 11
  IF ( ASSOCIATED(V%BComp%ProcPtr) ) ERROR STOP 13

  IF ( U%ChildId .NE. 2 ) ERROR STOP 22
  IF ( U%BComp%BaseId .NE. -1 ) ERROR STOP 21
  IF ( ASSOCIATED(U%BComp%ProcPtr) ) ERROR STOP 23


  END
