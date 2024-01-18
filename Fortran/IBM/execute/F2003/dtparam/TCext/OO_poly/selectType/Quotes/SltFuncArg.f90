! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltFuncArg.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltFuncArg.f
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
!*  TEST CASE NAME             : SltFuncArg
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 05, 2005
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
!*   The selector is a function return with associate name as argument 
!*    (297706)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4,*))  :: Arg
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4,*))  :: Arg
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

  END MODULE


  PROGRAM SltFuncArg
  USE M
  IMPLICIT NONE
  INTEGER :: Cnt=5

  ASSOCIATE( As => Fun(Child(4,20)(ChildId=2, BaseId=1)))

  END ASSOCIATE


  CONTAINS

  
  RECURSIVE FUNCTION Fun(Arg)
  CLASS(Zero(4,*)), TARGET ::Arg 
  CLASS(Zero(4,:)), POINTER :: Fun

  IF(Cnt .GT. 1 ) THEN
    Cnt = Cnt -1
    SELECT TYPE( AS => Fun(Arg))
    END SELECT
  ELSE

  ALLOCATE(Child(4,20) :: Fun)
 
  SELECT TYPE ( As0 => Arg )
    CLASS IS (Zero(4,*))
      SELECT TYPE (As => As0)
        TYPE IS (Child(4,*)) 
          IF ( As%Base%GetId() .NE. 1 ) STOP 34
          IF ( As%GetId()      .NE. 2 ) STOP 35
          IF ( As%BaseId       .NE. 1 ) STOP 36
          IF ( As%ChildId      .NE. 2 ) STOP 37
          CALL As%SetId()
          CALL As%Base%SetId()
          IF ( As%Base%GetId() .NE. -1 ) STOP 34
          IF ( As%GetId()      .NE. -2 ) STOP 35
          IF ( As%BaseId       .NE. -1 ) STOP 36
          IF ( As%ChildId      .NE. -2 ) STOP 37
       CLASS DEFAULT
          STOP 40
      END SELECT

    TYPE is (Base(4,*))
      STOP 32
    TYPE IS (Zero(4,*))
      STOP 38

  END SELECT

  END IF

  END FUNCTION 

  END
  
