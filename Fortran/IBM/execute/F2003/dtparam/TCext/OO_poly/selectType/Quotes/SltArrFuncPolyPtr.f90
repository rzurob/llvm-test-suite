! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrFuncPolyPtr.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltArrFuncPolyPtr.f
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
!*  TEST CASE NAME             : SltArrFuncPolyPtr
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
!*   The selector is a poly array pointer from function call
!*  
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base    ! (4)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
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
    CLASS(Base(4)), INTENT(INOUT) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4)), INTENT(INOUT)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrFuncPolyPtr
  USE M
  IMPLICIT NONE
  TYPE(Child(4)), TARGET :: V(2,2), W(2,2)
  INTEGER :: i
  CLASS(*), POINTER :: P1(:,:), P2(:,:)
  
  W%BaseId = -1
  W%ChildId = -2

  P1 => V
  P2 => W
  CALL Sub(P1, P2)

  CONTAINS
  
  SUBROUTINE Sub(Arg1, Arg2)
  CLASS(*) :: Arg1(:, :), Arg2(:, :)
  LOGICAL :: Mask(2, 2) = .true.

  Mask(1,2) = .false. 
  Mask(2,1) = .false.
 
  SELECT TYPE ( V=>MERGE(Arg1, Arg2, Mask) )
    CLASS DEFAULT
      SELECT TYPE (V)
        TYPE IS (Child(4))
          IF ( ANY(SHAPE(V)  .NE. (/2,2/)) ) STOP 20

          IF ( ANY(V%Base%GetId() .NE. RESHAPE((/ 1, -1, -1,  1/), (/2,2/))) ) STOP 34
          IF ( ANY(V%GetId()      .NE. RESHAPE((/ 2, -2, -2,  2/), (/2,2/))) ) STOP 35
          IF ( ANY(V%BaseId       .NE. RESHAPE((/ 1, -1, -1,  1/), (/2,2/))) ) STOP 36
          IF ( ANY(V%ChildId      .NE. RESHAPE((/ 2, -2, -2,  2/), (/2,2/))) ) STOP 37

       CLASS DEFAULT
          STOP 40
        CLASS is (Child(4))
          STOP 56
        TYPE is (Base(4))
          STOP 57
        TYPE IS (Zero(4))
          STOP 58
      END SELECT

  END SELECT

  END SUBROUTINE 
 
 
  END



