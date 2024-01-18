! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltAbsCom.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltAbsCom.f 
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
!*  TEST CASE NAME             : SltAbsCom
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
!*   The selector is specified with abstract component
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (20,4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Child) :: Abs    ! (20,4)
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(*,4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(*,4)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM SltAbsCom
  USE M
  IMPLICIT NONE

  CLASS(Base(:,4)), POINTER :: Ptr(:)
  TYPE(Child(20,4)), TARGET :: Tar(4)

  CLASS(Base(:,4)), POINTER :: Ptr1
  TYPE(Child(20,4)), TARGET :: Tar1

  Ptr1 => Tar1 

  SELECT TYPE ( Ptr1 )
  CLASS DEFAULT 
    STOP 20
  TYPE IS (Child(*,4))
    IF ( Ptr1%ChildId      .NE. 2 )  STOP 33
    IF ( Ptr1%GetId()      .NE. 2 )  STOP 33
    IF ( Ptr1%Base%BaseId  .NE. 1 )  STOP 34
!   IF ( Ptr1%Base%GetId() .NE. 1 )  STOP 34 C611
  END SELECT
  
  Ptr => Tar 

  SELECT TYPE ( As => Ptr )
  CLASS DEFAULT 
    STOP 40
  TYPE IS (Child(*,4))
    IF ( ANY(SHAPE(As) .NE. (/4/))  )   STOP 41
    IF ( LBOUND(As, 1) .NE. 1       )   STOP 42
    IF ( ANY(As%ChildId      .NE. (/2,2,2,2/)) )  STOP 43
    IF ( ANY(As%GetId()      .NE. (/2,2,2,2/)) )  STOP 43
    IF ( ANY(As%Base%BaseId  .NE. (/1,1,1,1/)) )  STOP 44
!   IF ( ANY(As%Base%GetId() .NE. (/1,1,1,1/)) )  STOP 44 !C611
  END SELECT
  
  END

