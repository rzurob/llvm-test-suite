! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  AttrOptionalArrVec.f  
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
!*  TEST CASE NAME             : AttrOptionalArrVec
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb 22, 2005
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
!*    
!*   The selector has an array section  
!*   the optional attribute   
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      private
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base), INTENT(INOUT) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base), INTENT(INOUT)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE


  END MODULE


  PROGRAM AttrOptionalArrVec
  USE M
  INTEGER :: i
  TYPE (Child) :: W(6)
  
  INTERFACE
    SUBROUTINE Sub(Arg, I, J)
    IMPORT Base 
    CLASS(Base), OPTIONAL :: Arg(I:J)
    INTEGER               :: I, J
    END SUBROUTINE 
  END INTERFACE

  W(::2) = Child(BaseID=-1, ChildID=-2)
  W(2::2)= Child(BaseID=-0, ChildID=-0)
  CALL Sub(W((/1,3,5/)), 3, 5)

  ! No thing can be changed
  IF ( ANY(W(::2)%BaseID        .NE. -1 )) STOP 40
  IF ( ANY(W(::2)%Base%GetId()  .NE. -1 )) STOP 41
  IF ( ANY(W(::2)%ChildID       .NE. -2 )) STOP 42
  IF ( ANY(W(::2)%GetId()       .NE. -2 )) STOP 43

  IF ( ANY(W(2::2)%BaseID        .NE. 0 )) STOP 45
  IF ( ANY(W(2::2)%Base%GetId()  .NE. 0 )) STOP 46
  IF ( ANY(W(2::2)%ChildID       .NE. 0 )) STOP 47
  IF ( ANY(W(2::2)%GetId()       .NE. 0 )) STOP 48

  END 

  SUBROUTINE Sub(Arg, I, J)
  USE M
  CLASS(Base), OPTIONAL :: Arg(I:J)
  INTEGER :: I, J

  IF ( .NOT. PRESENT(Arg) ) STOP  05

  IF ( .NOT. PRESENT (Arg) ) STOP 11
  IF ( ANY(LBOUND(Arg) .NE. (/I/) ) )     STOP 11
  IF ( ANY(SHAPE(Arg)  .NE. (/J-I+1/) ) ) STOP 12

  ASSOCIATE ( Arg => Arg((/I,I+1,J/)) )
  SELECT TYPE ( Arg )
  CLASS IS (Child)

    IF ( ANY(Arg%BaseID        .NE. -1 )) STOP 30
    IF ( ANY(Arg%Base%GetId()  .NE. -1 )) STOP 31
    IF ( ANY(Arg%ChildID       .NE. -2 )) STOP 32
    IF ( ANY(Arg%GetId()       .NE. -2 )) STOP 33
  
  CLASS DEFAULT
    STOP 99
  END SELECT

  END ASSOCIATE

  CALL Arg%SetID(Arg)

  END SUBROUTINE

