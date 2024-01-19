! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 27, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*  the length type parameter
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
    TYPE :: DT
      CHARACTER(4), ALLOCATABLE  :: CArr(:)
      CONTAINS
      PROCEDURE, PASS(Obj)   :: GetStr
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetStr(Num, Obj)
    CLASS(DT), INTENT(IN)    :: Obj
    INTEGER, INTENT(IN)      :: Num
    CHARACTER(LEN(Obj%CArr)) :: GetStr
      GetStr = Obj%CArr(Num)
    END FUNCTION

  END MODULE


  PROGRAM TypeParam1
  USE M
  IMPLICIT NONE

  TYPE(DT), TARGET   ::  DTV(3,3,3)
  CLASS(DT), POINTER :: Ptr(:,:,:)
  INTEGER :: S(2)=(/1,2/), I, J

  ! Ptr => Dtv
    ALLOCATE(Ptr(2,2,2), SOURCE=DT(CArr=(/"1234","4321"/)))

    SELECT TYPE (U => Ptr(S,S,S))
2     CLASS DEFAULT
        SELECT TYPE (U => U)
          CLASS IS (DT)
            IF (SIZE(U(2,2,2)%CArr) .NE. 2)      ERROR STOP 30
            IF (U(2,1,2)%CArr(1   ) .NE. "1234") ERROR STOP 31
            IF (U(1,1,2)%CArr(2)    .NE. "4321") ERROR STOP 32
            IF (LEN(U(2,2,2)%CArr)  .NE. 4)      ERROR STOP 33
          CLASS DEFAULT
            STOP 34
        END SELECT
    END SELECT
  END


