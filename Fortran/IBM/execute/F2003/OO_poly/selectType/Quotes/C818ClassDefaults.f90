! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 3, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C818
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    Nested select type constructs within class default bolck
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C818ClassDefaults
  IMPLICIT NONE

  CLASS(*), ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=1 )

A:  SELECT TYPE ( Var)
    CLASS DEFAULT A
B:    SELECT TYPE ( Var)
      CLASS DEFAULT B
C:      SELECT TYPE ( Var)
        CLASS DEFAULT C
D:        SELECT TYPE ( Var)
          CLASS DEFAULT D
          END SELECT D
        END SELECT C
      END SELECT B
    END SELECT A


  END

