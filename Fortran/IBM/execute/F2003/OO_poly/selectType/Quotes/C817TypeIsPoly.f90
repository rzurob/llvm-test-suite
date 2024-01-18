! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: C817TypeIsPoly.f 
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
!*  TEST CASE NAME             : C817TypeIsPoly
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
!*    The type guard TYPE IS is specified with the same type twice
!*    ()
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

  PROGRAM C817TypeIsPoly
  USE M
  IMPLICIT NONE
 
  CLASS(Level1), POINTER :: Var
  TYPE(Level1), TARGET  :: Tar
 
  Var => Tar 

  ASSOCIATE ( As => Var )
  SELECT TYPE ( As ) 

    TYPE IS (Level4)
      STOP 50
    TYPE IS (Level3)
      STOP 51
    TYPE IS (Level1)
      !STOP 52
      PRINT*, "E-level, still run here"
    TYPE IS (Level3)
      STOP 53
    TYPE IS (Level1)
      STOP 54
      
    CLASS DEFAULT
      STOP 30
  END SELECT 

  END ASSOCIATE 

  END

