! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltFuncElem.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltFuncElem.f
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
!*  TEST CASE NAME             : SltFuncElem
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 16, 2004
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
!*   The selector is a poly elemental func call.
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
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
 !    CLASS(Base), POINTER :: BasePtr => NULL()   ! due to C1272
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    INTERFACE ASSIGNMENT (=)
      ELEMENTAL SUBROUTINE ElemS(Arg1, Arg2)
        CLASS(*), INTENT(INOUT) :: Arg1 
        CLASS(*), INTENT(IN)    :: Arg2
      END SUBROUTINE
    END INTERFACE

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
    CLASS(Base(4))  :: Arg
      Arg%BaseId = -1
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4))  :: Arg
      Arg%ChildId = -2
    END SUBROUTINE

  END MODULE


  PROGRAM SltFuncElem
  USE M
  IMPLICIT  NONE

  !CLASS(*), ALLOCATABLE :: V1(:)
  CLASS(*), POINTER :: V1(:)
  TYPE(Child(4)) :: V2(5)
  INTEGER :: i

  ALLOCATE(Child(4) :: V1(5))
  V2 = (/(Child(4)(BaseID=i, ChildId=-i), i=1,5)/)

  SELECT TYPE ( V1 )
    CLASS DEFAULT
      STOP 30   
    TYPE is (Base(4))
      STOP 32
    CLASS IS (Child(4))
      V1 = V2
      DO i=1, 5
        IF ( V1(i)%Base%GetId() .NE.  i ) STOP 34 
        IF ( V1(i)%GetId()      .NE. -i ) STOP 35
        IF ( V1(i)%BaseId       .NE.  i ) STOP 36
        IF ( V1(i)%ChildId      .NE. -i ) STOP 37
      END DO
    CLASS IS (Zero(4))
      STOP 38 
  END SELECT

  END
  
    ELEMENTAL SUBROUTINE ElemS(Arg1, Arg2)
    USE M, ONLY : Child
    IMPLICIT NONE
    CLASS(*), INTENT(INOUT) :: Arg1 
    CLASS(*), INTENT(IN)    :: Arg2

      SELECT TYPE ( Arg1 )
        TYPE IS (Child(4))
          SELECT TYPE ( Arg2 )
            CLASS IS (Child(4))
              Arg1 = Arg2
          END SELECT
      END SELECT 
    END SUBROUTINE 


