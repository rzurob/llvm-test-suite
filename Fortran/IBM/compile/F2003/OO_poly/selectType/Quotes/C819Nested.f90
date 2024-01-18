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
!*    The same construct name is reused in nested select constructs
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM  C819Nested
  IMPLICIT NONE

  TYPE :: Base
  END TYPE

  CLASS(*),   POINTER :: Ptr
  TYPE(Base), TARGET  :: Tar

  Ptr => Tar
111 SELECT :  SELECT TYPE ( Ptr )
1   TYPE IS (Base) SELECT
      goto 11
11    PRINT*, "OK!"
2   CLASS IS (Base)
22    STOP 20
3   CLASS DEFAULT SELECT
4     SELECT : SELECT TYPE (Ptr)
        CLASS IS (Base)
         STOP 20
5     END SELECT SELECT
33    STOP 30
333 END SELECT  SELECT


  END

