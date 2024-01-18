! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 23, 2005
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
!*  Explicit dummy procedure - Characteristics
!*  Bound
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      CHARACTER(3) :: C
      TYPE(Base), POINTER :: BPtr
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base) :: Arg
        TYPE(Base):: IntF(2:4)
      END FUNCTION
    END INTERFACE

    INTERFACE
      FUNCTION IntF1(Arg)
      IMPORT
        TYPE(Base) :: Arg
        TYPE(Base):: IntF1(3)
      END FUNCTION
    END INTERFACE

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base) :: Arg
  TYPE(Base) :: ExtFun(3)
    ExtFun = Arg
  END FUNCTION


  PROGRAM Arg10
  USE M
  IMPLICIT NONE

  PROCEDURE(IntF) :: ExtFun
  PROCEDURE(IntF1), POINTER :: ProcPtr

  CALL IntSub(ExtFun)

  ProcPtr => ExtFun
  CALL IntSub1(ProcPtr)

  CONTAINS

    SUBROUTINE IntSub(Arg)
    IMPLICIT TYPE(Base)(A)
    PROCEDURE(IntF1) :: Arg
    TYPE(Base) :: V(3)
    TYPE(Base), TARGET :: Tar=Base("abc", NULL())
      V = Arg(Base("123", Tar))
      IF (ANY(V%C .NE. "123"))              STOP 11
      IF (.NOT. ASSOCIATED(V(2)%BPtr, Tar)) STOP 12
      IF (V(1)%BPtr%C .NE. "abc"  )         STOP 13
      IF (V(2)%BPtr%C .NE. "abc"  )         STOP 14
      IF (V(3)%BPtr%C .NE. "abc"  )         STOP 15
    END SUBROUTINE

    SUBROUTINE IntSub1(Arg)
    IMPLICIT TYPE(Base)(A)
    PROCEDURE(IntF), POINTER :: Arg
    TYPE(Base) :: V(3)
    TYPE(Base), TARGET :: Tar=Base("abc", NULL())
      V = Arg(Base("123", Tar))
      IF (ANY(V%C .NE. "123"))              STOP 21
      IF (.NOT. ASSOCIATED(V(2)%BPtr, Tar)) STOP 22
      IF (V(1)%BPtr%C .NE. "abc"  )         STOP 23
      IF (V(2)%BPtr%C .NE. "abc"  )         STOP 24
      IF (V(3)%BPtr%C .NE. "abc"  )         STOP 25
    END SUBROUTINE


  END

