! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 04, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  Complain  :
!*  " (S) Associate name fun is not associated with a variable or it is associated
!*    with a variable with a vector subscript.  It must not be redefined or
!*    become undefined."
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc9


  TYPE :: Base
  END TYPE

  CONTAINS

  FUNCTION Fun(Arg)
  TYPE(Base) :: Arg
  CLASS(Base), POINTER :: Fun
    ALLOCATE(Fun)
    SELECT TYPE( Fun )
      TYPE IS (Base)
        Fun=Arg
    END SELECT
  END FUNCTION

  END
