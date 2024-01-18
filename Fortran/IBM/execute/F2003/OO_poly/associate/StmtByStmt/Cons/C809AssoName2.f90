! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  C809AssoName2.f  
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
!*  TEST CASE NAME             : C809AssoName2
!*  TEST CASE TITLE            : C809
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct. 20, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name 
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
!*     The associate name must only be declared once in the ASSOCIATE statement  
!*     Selector is a dummy with the same name 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
 
  MODULE M

    TYPE, ABSTRACT :: Base
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: Id = 0
    CONTAINS
      PROCEDURE, NOPASS :: PrintType => PrintChild 
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%Id
    END FUNCTION

  END MODULE

  PROGRAM C809AssoName1
  USE M
  IMPLICIT NONE
  TYPE(Child) :: V = Child(1)

    V = F(V) 
     
    CONTAINS

    FUNCTION F(As)
    TYPE(Child) :: As, F

    F = Child(1)

    ASSOCIATE ( As => As  )
      IF (As%Id .NE. 1) STOP 50
      IF (As%GetId() .NE. 1) STOP 51
      As%Id = 2

      ASSOCIATE ( As => As  )
        IF (As%Id .NE. 2) STOP 52
        IF (As%GetId() .NE. 2) STOP 53
      END ASSOCIATE
    END ASSOCIATE

    IF (As%Id .NE. 2) STOP 52
    IF (As%GetId() .NE. 2) STOP 53

    END FUNCTION

  END

