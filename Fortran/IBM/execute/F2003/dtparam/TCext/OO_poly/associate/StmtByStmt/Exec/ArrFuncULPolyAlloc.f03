! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrFuncULPolyAlloc.f
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
!*    The selector is a function call returning an unlimited poly
!*    allocatable array
!*
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseID=1
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      TYPE(Base(K1)) :: BS
      CLASS(Base(K1)), POINTER :: BSPtr
      INTEGER(K1) :: ChildID=2
    END TYPE

  CONTAINS

    FUNCTION ReturnArr(Arg)
    CLASS (*) :: Arg(:)
    CLASS (*), POINTER :: ReturnArr(:)
      ALLOCATE(ReturnArr(SIZE(Arg)), SOURCE=Arg(:))
    END FUNCTION

  END MODULE

  PROGRAM ArrFuncULPolyAlloc
  USE M
  IMPLICIT NONE
  INTEGER :: i
  CLASS(*), ALLOCATABLE :: Arr(:)

  ALLOCATE(Arr(5), SOURCE= (/(Child(4)(          &
  &                  Base   =Base(4)(BaseID=0),  &
  &                  BS     =Base(4)(BaseID=-1), &
  &                  BSPtr  =NULL(),          &
  &                  ChildID=-2)              &
  &            , i=1,5)/) )

  ASSOCIATE ( As => ReturnArr(Arr(1::1)) )

    IF ( ANY (LBOUND(As)  .NE. (/1/) ) )   ERROR STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/5/) ) )   ERROR STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/5/) ) )   ERROR STOP 32

    SELECT TYPE ( As => As(1:))
    TYPE IS (Child(4))

      IF ( ANY (As%ChildID    .NE. -2 ))    ERROR STOP 33
      IF ( ANY (As%BS%BaseID  .NE. -1 ))    ERROR STOP 34
      IF ( ANY (As%BaseID     .NE.  0 ))    ERROR STOP 35

      DO i =1, SIZE(As)
        IF ( ASSOCIATED(As(i)%BSPtr)  )     ERROR STOP 36

        ASSOCIATE (As => As(i)%BS )
          IF ( As%BaseID  .NE. -1 )          ERROR STOP 37
        END ASSOCIATE

      END DO

    CLASS DEFAULT
      STOP 88
    END SELECT

  END ASSOCIATE

  END
