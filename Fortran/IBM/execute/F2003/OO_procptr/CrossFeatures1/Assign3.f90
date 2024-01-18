! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 16, 2005
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
!*  A derived-type intrinsic assignment
!*  (304716)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
       CHARACTER(*) :: Arg
       CHARACTER(LEN(Arg)) :: CToc
      END FUNCTION
    END INTERFACE

    TYPE :: Base
      PROCEDURE(CToC), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT
      INTEGER :: Id
      TYPE(Base), POINTER :: BComp
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE  PToP
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

    SUBROUTINE PToP (Arg1, Arg2)
    TYPE(DT), INTENT (OUT) :: Arg1
    TYPE(DT), INTENT (IN)  :: Arg2
      Arg1%Id  = Arg2%Id
      Arg1%BComp => Arg2%BComp
    END SUBROUTINE

  END MODULE


  PROGRAM Assign3
  USE M
  IMPLICIT NONE

  TYPE (DT) :: V
  TYPE (Base),     TARGET  :: BTar
  PROCEDURE(CToC), POINTER :: ProcPtr
  CHARACTER(1025)          :: Str=CHAR(40)

  ProcPtr => RetPtr(Fun)
  BTar = Base(RetPtr(Fun))
  V = DT(-1, BTar)

  IF ( V%Id .NE. -1 ) ERROR STOP 11
  IF ( .NOT. ASSOCIATED(V%BComp, BTar) )  ERROR STOP 12
  IF ( .NOT. ASSOCIATED(V%BComp%ProcPtr, RetPtr(Fun)) )  ERROR STOP 12

  IF (V%Bcomp%ProcPtr("ABC") .NE. "ABC" ) ERROR STOP 14

  IF (V%BComp%ProcPtr("") .NE. "" ) ERROR STOP 15

  IF (V%BComp%ProcPtr(Str) .NE. Str ) ERROR STOP 15

  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(CToC), POINTER :: RetPtr
  PROCEDURE(CToC) :: Arg
    RetPtr => Arg
  END FUNCTION

  END

