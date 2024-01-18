! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/VarDummyPoly.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  VarDummyPoly.f  
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
!*  TEST CASE NAME             : VarDummyPoly 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 02, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a polymorphic dummy
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      CLASS(Base(K1)),  POINTER :: BaseComp 
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child(4)) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base(4))  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM VarDummyPoly
  USE M
  CLASS(Base(4)), POINTER :: U

  ALLOCATE(Child(4) :: U)
  CALL Sub(U, U)
 
  CONTAINS

  SUBROUTINE Sub(Arg1, Arg2)
  CLASS(Base(4)) :: Arg1
  CLASS(*)    :: Arg2

  ASSOCIATE ( As => Arg1 )
    IF ( As%GetID() .NE. 2) STOP 50 
    IF ( As%BaseId  .NE. 1) STOP 51 

    ASSOCIATE ( As1 => As%BaseId )
       IF ( As1 .NE. 1) STOP 52 
    END ASSOCIATE
   
    IF ( .NOT. SAME_TYPE_AS(As, Child(4)(BaseComp=NULL())) ) STOP 53

    SELECT TYPE ( As )
      TYPE IS (Child(4))
        ALLOCATE(Child(4) :: As%BaseComp)

        IF ( As%BaseComp%BaseId  .NE. 1) STOP 54
        IF ( As%BaseComp%GetId() .NE. 2) STOP 55

        IF ( As%GetID() .NE. 2) STOP 56
        IF ( As%ChildId .NE. 2) STOP 57
     
        As%BaseId  = -1  !Test Arg2
        As%ChildId = -2

      CLASS DEFAULT
        STOP 70
    END SELECT

  END ASSOCIATE

  !Unlimited poly Arg2
  ASSOCIATE ( As => Arg2 )

    SELECT TYPE ( As )
      TYPE IS (Child(4))
        IF ( As%GetID() .NE. -2)      STOP 40 
        IF ( As%ChildId .NE. -2)      STOP 41 
        IF ( As%BaseId  .NE. -1)      STOP 42 
        IF ( As%Base%GetId() .NE. -1) STOP 43 
       
        ASSOCIATE ( As1 => As%BaseId )
         IF ( As1 .NE. -1) STOP 44 
        END ASSOCIATE

        IF ( .NOT. SAME_TYPE_AS(As, Child(4)(BaseComp=NULL())) ) STOP 45 
        IF ( .NOT. SAME_TYPE_AS(As%BaseComp, As) ) STOP 46 

        ALLOCATE(As%BaseComp)
        IF ( As%BaseComp%BaseId  .NE. 1) STOP 47 
        IF ( As%BaseComp%GetId() .NE. 1) STOP 48 
        DEALLOCATE(As%BaseComp)

        IF ( As%GetID() .NE. -2) STOP 49 
        IF ( As%ChildId .NE. -2) STOP 30 

      CLASS DEFAULT
        STOP 80
    END SELECT

  END ASSOCIATE

  END SUBROUTINE

  END
