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
!*  Procedure pointer components - sequence
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      SEQUENCE
      CHARACTER(3)                 :: C
      PROCEDURE(LOGICAL(1)), NOPASS, POINTER :: ProcPtr
    END TYPE

    INTERFACE
      FUNCTION ExtFun(C, ProcPtr)
        IMPORT
        TYPE(Base)           :: ExtFun
        CHARACTER(3)         :: C
        PROCEDURE(LOGICAL(1)), POINTER :: ProcPtr
      END FUNCTION
    END INTERFACE

  CONTAINS

  FUNCTION ModFun()
  LOGICAL(1) :: ModFun
    ModFun = .TRUE.
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(C, ProcPtr)

  TYPE :: Base
    SEQUENCE
    CHARACTER(3)                 :: C
    PROCEDURE(LOGICAL(1)), NOPASS, POINTER :: ProcPtr
  END TYPE

  TYPE(Base)           :: ExtFun
  CHARACTER(3)         :: C
  PROCEDURE(LOGICAL(1)), POINTER :: ProcPtr
    ExtFun%ProcPtr => ProcPtr
    ExtFun%C = C

  END FUNCTION


  PROGRAM StrComp8
  USE M
  IMPLICIT TYPE(Base)(P)

  TYPE(Base)  :: U
  LOGICAL     :: L=.FALSE.

  U = Base("123", ModFun )
  IF ( U%C .NE. "123" )                       ERROR STOP 21
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModFun ))  ERROR STOP 22
  IF ( .NOT. U%ProcPtr())                     ERROR STOP 23

  U =  Base("", NULL() )
  IF ( U%C .NE. "" )        ERROR STOP 31
  IF ( ASSOCIATED(U%ProcPtr )) ERROR STOP 32


  END
