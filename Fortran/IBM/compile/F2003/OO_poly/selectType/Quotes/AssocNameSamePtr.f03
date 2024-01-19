! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 2, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Associate name
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
!*
!*   The associate name is the same as the selector
!*   Test the pointer attribute
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  AssocNameSamePtr
  IMPLICIT NONE

  TYPE :: Base
  END TYPE

  CLASS(*),   POINTER :: Ptr
  TYPE(Base), TARGET  :: Tar

  Ptr => Tar
  SELECT TYPE ( Ptr => Ptr )
    TYPE IS (Base)
      PRINT*, "OK!"
      Ptr => Tar
    CLASS IS (Base)
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT

  SELECT TYPE ( Ptr )
    TYPE IS (Base)
      PRINT*, "OK!"
      Ptr => Tar
    CLASS IS (Base)
      STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT


  END

