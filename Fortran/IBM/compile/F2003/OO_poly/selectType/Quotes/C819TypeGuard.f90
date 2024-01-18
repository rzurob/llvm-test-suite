! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Constraint C819
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
!*    The inconsistency on construct name
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM C819TypeGuard
  IMPLICIT NONE

  TYPE :: Base
  END TYPE

  CLASS(*),   POINTER :: Ptr
  TYPE(Base), TARGET  :: Tar

  Ptr => Tar

  SELECT TYPE ( Ptr )
1   TYPE IS (Base) TYPE
11    PRINT*, "OK!"
2   CLASS IS (Base)CLASS
22    STOP 20
3   CLASS DEFAULT DEFAULT
33    STOP 30
 END SELECT


  END

