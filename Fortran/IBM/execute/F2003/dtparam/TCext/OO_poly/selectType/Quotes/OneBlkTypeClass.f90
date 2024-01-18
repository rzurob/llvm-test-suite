! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=base /tstdev/OO_poly/selectType/Quotes/OneBlkTypeClass.f
! opt variations: -qnok -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: OneBlkTypeClass.f  
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
!*  TEST CASE NAME             : OneBlkTypeClass
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 13, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : ONLY ONE TYPE/CLASS IS BLOCK 
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
!*   The type /class is bolck is specified with the same type spec 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M

    TYPE, ABSTRACT ::  Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero) :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE



  PROGRAM OneBlkTypeClass 
  USE M
  IMPLICIT NONE

  INTEGER :: Visited = 0 

  IF (Fun(Base(4,20)())) THEN
    IF (Visited .NE. 1 ) THEN
       STOP 111
    END IF
  END IF

  Visited = 0
  IF (Fun(Child(4,20)())) THEN
    IF (Visited .NE. 1 ) THEN
       STOP 112
    END IF
  END IF

  Visited = 0
  IF (Fun(1_1)) THEN
    IF (Visited .NE. 1 ) THEN
       STOP 113
    END IF
  END IF

  CONTAINS

  LOGICAL FUNCTION Fun(Arg)
  CLASS(*) :: Arg

  SELECT TYPE ( Arg )
    CLASS DEFAULT
      Visited = Visited + 1 
    TYPE IS (Child(4,*))
      Visited = Visited + 1
    CLASS IS (Base(4,*))
      Visited = Visited + 1
    CLASS IS (Child(4,*))
      Visited = Visited + 1
    TYPE IS (Base(4,*))
      Visited = Visited + 1
  END SELECT

  Fun = .TRUE.

  END FUNCTION

  END

