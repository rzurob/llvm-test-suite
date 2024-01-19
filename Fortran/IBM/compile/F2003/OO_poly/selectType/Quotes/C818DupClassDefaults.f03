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
!*     duplicated class default blocks
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C818DupClassDefaults
  IMPLICIT NONE

  CLASS(*), ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=1 )

A:  SELECT TYPE ( Var)
    CLASS DEFAULT A
    CLASS DEFAULT A
B:    SELECT TYPE ( Var)
      CLASS DEFAULT B
      CLASS DEFAULT B
C:      SELECT TYPE ( Var)
        CLASS DEFAULT C
        CLASS DEFAULT C
D:        SELECT TYPE ( Var)
          CLASS DEFAULT D
          CLASS DEFAULT D
          END SELECT D
        END SELECT C
      END SELECT B
    END SELECT A


  END

