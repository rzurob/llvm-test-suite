! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrFuncPtr1.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltArrFuncPtr1.f
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltArrFuncPtr1
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 18, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     
!*   The selector is a function call returing a poly array pointer
!*   The function is a dummy procedure 
!*    (ICE)
!*    (Wrong result-301404)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 MODULE M

    TYPE  :: Zero(K1)    ! (4)
        INTEGER, KIND :: K1
    CONTAINS
      PROCEDURE, NoPASS   :: SetId
      PROCEDURE, NoPASS   :: ReturnChild 
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
   !  PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
   !  PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4))  :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetId(Arg)
    CLASS(Base(4))  :: Arg(:,:)
      SELECT TYPE (Arg)
        TYPE IS (Base(4))
          Arg%BaseId =  -Arg%BaseId
        TYPE IS (Child(4))
          Arg%ChildId =  -Arg%ChildId
      END SELECT
    END SUBROUTINE

    FUNCTION ReturnChild(Arg)
    CLASS(Zero(4)), INTENT(IN) :: Arg(:,:)
    Type(Child(4))   :: ReturnChild(SIZE(Arg, 1), SIZE(Arg, 2))
      SELECT TYPE(Arg)
        TYPE IS (Child(4))
          ReturnChild = Arg
        CLASS DEFAULT
         ! STOP 111
      END SELECT
    END FUNCTION 

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4))  :: Arg(:,:)
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

    FUNCTION Obj(Arg, Func)
    IMPLICIT NONE
    CLASS(Zero(4)), INTENT(IN) :: Arg(:,:)
    PROCEDURE(ReturnChild)  :: Func
    CLASS(*), POINTER :: Obj(:,:)
      ALLOCATE(Child(4) :: Obj(SIZE(Arg, 1), SIZE(Arg, 2)))
      SELECT TYPE ( Obj)
        TYPE IS (Child(4))
          Obj = Func(Arg)
        CLASS DEFAULT
          STOP 111
      END SELECT
    END FUNCTION

  END MODULE


  PROGRAM SltArrFuncPtr1
  USE M
  IMPLICIT NONE
  TYPE(Child(4)) :: V(3,3)
  INTEGER :: B1(3)=(/1,2,3/)
  INTEGER :: B2(3)=(/1,2,3/)

  SELECT TYPE ( As => Obj(V, ReturnChild) )
  CLASS DEFAULT

      SELECT TYPE (As)
        TYPE IS (Child(4))

          IF ( ANY (SHAPE(As) .NE. (/3,3/) )) STOP 20
          IF ( ANY (As%Base%GetId() .NE. 1 )) STOP 34
          IF ( ANY (As%GetId()      .NE. 2 )) STOP 35
          IF ( ANY (As%BaseId       .NE. 1 )) STOP 36
          IF ( ANY (As%ChildId      .NE. 2 )) STOP 37

          CALL As%SetId(As)
          CALL As%SetId(As%Base)

          IF ( ANY (As%Base%GetId() .NE. -1 )) STOP 44
          IF ( ANY (As%GetId()      .NE. -2 )) STOP 45
          IF ( ANY (As%BaseId       .NE. -1 )) STOP 46
          IF ( ANY (As%ChildId      .NE. -2 )) STOP 47

       CLASS DEFAULT
          STOP 40
       TYPE is (Base(4))
          STOP 32
       TYPE IS (Zero(4))
          STOP 38
      END SELECT

  END SELECT

  END



