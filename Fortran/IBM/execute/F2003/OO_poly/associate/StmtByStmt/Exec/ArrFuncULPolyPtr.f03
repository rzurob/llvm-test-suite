! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 14, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a function call returning an unlimited poly array
!*
!*    (Wrong syntax check-301450)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base
      INTEGER :: BaseID=1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      TYPE(Base) :: BS
      CLASS(Base), POINTER :: BSPtr
      INTEGER :: ChildID=2
    END TYPE

  CONTAINS

    FUNCTION ReturnArr(Arg)
    TYPE (Child) :: Arg(:)
    CLASS (*), POINTER :: ReturnArr(:)
      ALLOCATE(ReturnArr(SIZE(Arg)), SOURCE=Arg)
    END FUNCTION

  END MODULE

  PROGRAM ArrFuncULPolyPtr
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE(Child) :: Arr(555)

  ASSOCIATE ( As => &
  &  ReturnArr((/ (Child(Base=Base(BaseID=0), &
  &                      BS=Base(BaseID=-1),  &
  &                      BSPtr=NULL(),        &
  &                      ChildID=-2)          &
  &            , i=1,555)/) ) )

    IF ( ANY (LBOUND(As)  .NE. (/1/) ) )     ERROR STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/555/) ) )   ERROR STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/555/) ) )   ERROR STOP 32

    SELECT TYPE (As => As)
    TYPE IS (Child)

      IF ( ANY (As%ChildID    .NE. -2 ))     ERROR STOP 33
      IF ( ANY (As%BS%BaseID  .NE. -1 ))     ERROR STOP 34
      IF ( ANY (As%BaseID     .NE.  0 ))     ERROR STOP 35

      ASSOCIATE (As => As)
        DO i =1, SIZE(As)

          IF ( ASSOCIATED(As(i)%BSPtr))      ERROR STOP 35

          ASSOCIATE (As => As(i)%BS )
            IF ( As%BaseID  .NE. -1 )        ERROR STOP 37
          END ASSOCIATE
        END DO
      END ASSOCIATE

    CLASS DEFAULT
      STOP 88
    END SELECT

  END ASSOCIATE

  END

