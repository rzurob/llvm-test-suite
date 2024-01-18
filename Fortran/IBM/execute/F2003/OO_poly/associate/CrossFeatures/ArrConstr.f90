! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ArrConstr.f
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
!*  TEST CASE NAME             : ArrConstr 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 07, 2005
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
!*    The selector is a nested arr constructor 
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

     TYPE, ABSTRACT :: Base
      INTEGER :: BaseId = 1
      CLASS(*), ALLOCATABLE :: Unknown(:) 
      CONTAINS
      PROCEDURE,nopass :: Bnd
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    END TYPE

    CONTAINS
  
    ELEMENTAL FUNCTION Bnd(Arg)
    INTEGER, INTENT(IN) :: Arg
    INTEGER :: Bnd
      Bnd =Arg
    END FUNCTION

  END MODULE
  
  PROGRAM DerTypeArrConstr
  USE M, DT=>Child
  IMPLICIT NONE

  ASSOCIATE( As => (/DT(Unknown=(/"123","123"/)),DT(Unknown=(/"123","123"/)) /) )

    IF ( ANY(SHAPE(As)  .NE. (/2/) ) ) STOP 20
    IF ( ANY(As%BaseID  .NE. 1 ) )     STOP 21
    IF ( ANY(As%ChildID .NE. 2 ) )     STOP 22

    IF ( As%Bnd(2)  .NE. 2  )          STOP 23

    SELECT TYPE ( As => As(2)%Unknown  )
    TYPE IS (CHARACTER(*))
      IF ( ANY(SHAPE(As) .NE. (/2/) ) )  STOP 30
      IF ( ANY(As        .NE. "123" ) )  STOP 31
    CLASS DEFAULT
      STOP 33     
    END SELECT 

  END ASSOCIATE

  END


