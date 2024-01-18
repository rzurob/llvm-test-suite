! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  redherring.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp C811Func.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C811Func
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 2, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C811 
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
!*    The selector is a function call  without ssociate-name => 
!*    
!*    (Passing-301383)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM C811Func
  IMPLICIT NONE

  TYPE :: Base
  END TYPE
 
  SELECT TYPE ( Fun(Base()) )
    TYPE IS (Base)
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT 
  STOP 40

  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(Base) :: Arg
  CLASS(Base), POINTER :: Fun
    ALLOCATE(Fun)
    SELECT TYPE( Fun )
      TYPE IS (Base)
        Fun=Arg
      CLASS DEFAULT 
        STOP 22
    END SELECT
  END FUNCTION

  END

