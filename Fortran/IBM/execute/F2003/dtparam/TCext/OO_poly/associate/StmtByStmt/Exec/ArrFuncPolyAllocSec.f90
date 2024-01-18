! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrFuncPolyAllocSec.f
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
!*    The selector is a function call returning an allocatable array section
!*
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      SEQUENCE
      INTEGER(K1)   :: BaseID=1
    END TYPE

    TYPE :: Child(K2)    ! (4)
      INTEGER, KIND  :: K2
      SEQUENCE
      TYPE(Base(K2)) :: BS
      INTEGER(K2)    :: ChildID=2
    END TYPE

  CONTAINS

    FUNCTION ReturnArr(Arg)
    TYPE (Child(4)) :: Arg(:)
    TYPE (Child(4)), ALLOCATABLE :: ReturnArr(:)
      ALLOCATE(ReturnArr(SIZE(Arg)), SOURCE=Arg)
    END FUNCTION
  END MODULE

  PROGRAM ArrFuncPolyAllocSec
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE(Child(4)) :: Arr(555)

  ASSOCIATE ( As => &
  &  ReturnArr((/ (Child(4)(BS=Base(4)(BaseID=-1), ChildID=-2), i=1,555)/) ) )

    IF ( ANY (LBOUND(As)  .NE. (/1/) ) )     ERROR STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/555/) ) )   ERROR STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/555/) ) )   ERROR STOP 32
    IF ( ANY (As%ChildID    .NE. -2 ))         ERROR STOP 33
    IF ( ANY (As%BS%BaseID  .NE. -1 ))         ERROR STOP 34

    ASSOCIATE (A => As(1::2) )
      IF ( ANY (As%ChildID    .NE. -2 ))         ERROR STOP 33
      IF ( ANY (As%BS%BaseID  .NE. -1 ))         ERROR STOP 34
    END ASSOCIATE
  END ASSOCIATE

  END

