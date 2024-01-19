! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 18, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Procedure pointer components
!*  (FaileD - 267618)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      CHARACTER(3) :: C
      PROCEDURE(CHARACTER(3)), NOPASS, POINTER :: ProcPtr
    END TYPE

    INTERFACE Base
      FUNCTION ExtFun(C, ProcPtr)
        IMPORT
        TYPE(Base)                    :: ExtFun
        CHARACTER(3)                  :: C
        PROCEDURE(CHARACTER(3))       :: Proc
      END FUNCTION
    END INTERFACE

  CONTAINS

  FUNCTION CFun()
  CHARACTER(3) :: CFun
    CFun = "OK!"
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(C, Proc)
  USE M, ONLY: Base

  TYPE(Base)                 :: ExtFun
  CHARACTER(3)               :: C
  PROCEDURE(CHARACTER(3))    :: Proc
    ExtFun%ProcPtr => Proc
    ExtFun%C = C
  END FUNCTION

  PROGRAM StrComp8
  USE M, ONLY: Base, CFun
  IMPLICIT CHARACTER(3)(P)

  TYPE(Base)  :: U

  U = Base("123", NULL())
  IF ( U%C .NE. "123"  )  ERROR STOP 21
  IF ( ASSOCIATED(U%ProcPtr))  ERROR STOP 22

  U = Base("321", CFun)   ! reference to the interface base
  IF ( U%C .NE. "321"  )  ERROR STOP 31
  IF ( U%ProcPtr("321") .NE. "OK!" )  ERROR STOP 32

  END

