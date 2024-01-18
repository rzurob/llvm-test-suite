! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_poly/selectType/Quotes/SltArrFuncZeroSiz.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 17, 2005
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
!*   The selector is a function call returing a poly array of zero
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4,20)
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = 2
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = 1
    END FUNCTION

  END MODULE


  PROGRAM SltArrFuncZeroSiz
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)) :: V(3,3)
  INTEGER :: B1(3)=(/1,2,3/)
  INTEGER :: B2(3)=(/1,2,3/)

  SELECT TYPE ( Arg => Fun(Arg=Fun(Arg=V)) )
  CLASS DEFAULT
  ASSOCIATE( As => Arg )

      SELECT TYPE (As )
        TYPE IS (Child(4,*))

          IF ( ANY (SHAPE(As) .NE. (/3,3/) )) STOP 20
          IF ( ANY (As%Base%GetId() .NE. 1 )) STOP 34
          IF ( ANY (As%GetId()      .NE. 2 )) STOP 35

       CLASS DEFAULT
          STOP 40
       TYPE is (Base(4,*))
          STOP 32
       TYPE IS (Zero(4,*))
          STOP 38
      END SELECT

  END ASSOCIATE
  END SELECT

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*), TARGET  :: Arg(:,:)
  CLASS(*), ALLOCATABLE :: Fun(:,:)

  ALLOCATE(Fun(SIZE(Arg,1), SIZE(Arg,2)), SOURCE=Arg)

  END FUNCTION
  END



