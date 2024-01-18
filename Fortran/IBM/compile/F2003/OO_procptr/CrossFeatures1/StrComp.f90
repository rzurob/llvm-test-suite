! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 17, 2005
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
!*  Structure component
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


    TYPE :: Base
      PROCEDURE(COMPLEX), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT
      INTEGER :: Id=0
      TYPE(Base), POINTER :: BComp
    END TYPE


    CONTAINS

    FUNCTION Fun(Arg)
    COMPLEX :: Arg
    COMPLEX :: Fun
      Fun = Arg
    END FUNCTION


  END MODULE


  PROGRAM StrComp
  USE M
  IMPLICIT NONE

  TYPE(DT)   :: D(512)

  D%BComp%ProcPtr => Fun

  D(1::2)%BComp%ProcPtr => Fun

  D(2:2)%BComp%ProcPtr => Fun

  D(3:2)%BComp%ProcPtr => Fun

  END

