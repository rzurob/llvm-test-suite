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
!*  (Coredump)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
       CHARACTER(*) :: Arg
       CHARACTER(LEN(Arg)) ::CToC
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
      MODULE PROCEDURE PToP
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


  PROGRAM Assign2
  USE M
  IMPLICIT NONE

  TYPE (DT)                :: V
  TYPE (Base), TARGET      :: BTar
  PROCEDURE(CToC), POINTER :: ProcPtr
  CHARACTER(1000000)       :: Str=CHAR(40)

  BTar = Base(Fun)
  ProcPtr => Fun
  V = DT(-1, BTar)

  IF ( V%Id .NE. -1 ) STOP 11
  IF ( .NOT. ASSOCIATED(V%BComp%ProcPtr, Fun) ) STOP 13

  IF (V%BComp%ProcPtr("ABC") .NE. "ABC" ) STOP 14

  IF (V%BComp%ProcPtr("")   .NE. "" )     STOP 15

  IF (V%BComp%ProcPtr(Str)   .NE. Str )   STOP 15


  END

