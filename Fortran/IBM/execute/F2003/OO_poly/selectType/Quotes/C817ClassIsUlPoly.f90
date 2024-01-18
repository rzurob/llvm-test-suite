! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: C817ClassIsUlPoly.f 
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
!*  TEST CASE NAME             : C817ClassIsUlPoly
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 3, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C817 
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
!*    The type guard CLASS IS is specified with the same type twice
!*    The selector is unlimited poly
!*    (E-level, compilation successful)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Level0
    END TYPE

    TYPE, EXTENDS(Level0) :: Level1
      INTEGER :: Level1Id = 1
    END TYPE

    TYPE, EXTENDS(Level1) :: Level2
      INTEGER :: Level2Id = 2
    END TYPE

    TYPE, EXTENDS(Level2) :: Level3
      INTEGER :: Level3Id = 3
    END TYPE

    TYPE, EXTENDS(Level3) :: Level4
      INTEGER :: Level4Id = 4
    END TYPE

  END MODULE

  PROGRAM C817ClassIsPoly
  USE M
  IMPLICIT NONE
 
  CLASS(*), POINTER :: Ptr 
  INTEGER, TARGET  :: Tar

  Ptr  => Tar 

  SELECT TYPE ( Ptr ) 
    TYPE IS (Level4)
      STOP 50
    CLASS IS (Level4)
      STOP 51
    CLASS IS (Level1)
      STOP 53
    CLASS IS (Level4)
      STOP 55
    CLASS IS (Level1)
      STOP 55
      
    CLASS DEFAULT
    ! STOP 30
  END SELECT 

  SELECT TYPE ( Ptr )
    TYPE IS (INTEGER)
    ! STOP 42
      PRINT*, "Still going here!"
    TYPE IS (INTEGER(4))
      STOP 43
  END SELECT

  SELECT TYPE ( Ptr )
    TYPE IS (REAL(8))
      STOP 32
    TYPE IS (DOUBLE PRECISION)
      STOP 33
  END SELECT

  SELECT TYPE ( Ptr )
    TYPE IS (LOGICAL)
      STOP 62
    TYPE IS (LOGICAL(4))
      STOP 63
  END SELECT

  END

