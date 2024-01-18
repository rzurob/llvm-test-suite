! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltTypeBnd.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltTypeBnd.f
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
!*  TEST CASE NAME             : SltTypeBnd
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 14, 2004
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
!*   The type spec is specified with a type with variuos bindings 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND        :: K1
      INTEGER(K1), PRIVATE :: BaseId=-1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Base) :: Base1    ! (4)
      INTEGER(K1) :: Base1Id=1
    CONTAINS
      PROCEDURE(GetBase1Interface), PASS, DEFERRED, PRIVATE   :: GetIdx
      PROCEDURE, PASS  :: GetBase1Id
    END TYPE

    TYPE, EXTENDS(Base1) :: Child    ! (4)
    ! PRIVATE
      INTEGER(K1) :: ChildId=2
      TYPE(Base(K1))  :: Base1Comp=Base(K1)(BaseId=0)
    CONTAINS
      PROCEDURE, PASS  :: GetId => GetChildId
      PROCEDURE, PASS  :: GetIdx => GetChildId ! bypass the original private attr ?
    END TYPE

    INTERFACE 
      ELEMENTAL FUNCTION GetBase1Interface(Arg)
        IMPORT Base1 
        CLASS(Base1(4)), INTENT(IN) :: Arg
        INTEGER      :: GetBase1Interface
      END FUNCTION
    END INTERFACE

    CONTAINS

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    FUNCTION GetBase1Id(Arg)
    CLASS(Base1(4))  :: Arg
    INTEGER      :: GetBase1Id
      GetBase1Id = Arg%Base1Id
    END FUNCTION

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

  END MODULE

  PROGRAM SltTypeBnd
  USE M
  IMPLICIT NONE

  CLASS(*) ,ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=Child(4)())

  SELECT TYPE (Var)
   CLASS IS (Base(4))
     STOP 20
   TYPE IS (Base(4))
     STOP 21
   CLASS IS (Child(4))
     STOP 22
   TYPE IS (Child(4))
     IF (Var%GetIdx()           .NE. 2 )    STOP 31
     IF (Var%GetId()            .NE. 2 )    STOP 32
     IF (Var%ChildId            .NE. 2 )    STOP 33
     IF (Var%Base1Id            .NE. 1 )    STOP 34
     IF (Var%GetBase1Id()       .NE. 1 )    STOP 35 
     IF (Var%Base%GetId()       .NE. -1 )   STOP 36
     IF (Var%Base1%Base%GetId() .NE. -1 )   STOP 37
     IF (Var%Base1%Base%GetId() .NE. -1 )   STOP 38
!    IF (Var%Base1%BaseId       .NE. -1 )   STOP 30 ! Private
     IF (Var%Base1%Base1Id      .NE.  1 )   STOP 39

!    IF (Var%BaseId  .NE. -1 )      STOP 31  ! Private
!    IF (Var%Base1%GetBase1Id() .NE. 1 ) STOP 32 ! this is wrong
  END SELECT

  END
