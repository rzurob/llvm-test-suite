! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignGen1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignGen1.f
!*
!*  DATE                       : Mar. 27, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  Generic interface
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    CONTAINS

    FUNCTION Int1(Arg)
    INTEGER(1) :: Int1, Arg
      Int1 = 1_1
    END FUNCTION

    FUNCTION Int2(Arg)
    INTEGER(2) :: Int2, Arg
      Int2 = 2_2
    END FUNCTION

    FUNCTION Int8(Arg)
    INTEGER(8) :: Int8, Arg
      Int8 = 8_8
    END FUNCTION

  END MODULE

  FUNCTION Int(Arg)
  INTEGER :: Int, Arg
    Int = Arg
  END FUNCTION


  PROGRAM PtrAssignGen1
  USE M
  IMPLICIT NONE
  PROCEDURE(Int2), POINTER :: ProcInt2


  ProcInt2 => Int2
  CALL IntSub(Int1, ProcInt2 )

  CONTAINS

  SUBROUTINE  IntSub(Proc, ProcPtr)
  PROCEDURE(Int1)          :: Proc
  PROCEDURE(Int2), POINTER :: ProcPtr


  INTERFACE Int

    FUNCTION Int(Arg)
      INTEGER :: Int, Arg
    END FUNCTION

!  Comment out parts as the functionality has not been implemented yet.

!          PROCEDURE  Proc
!          PROCEDURE  ProcPtr
    MODULE PROCEDURE  Int8
!          PROCEDURE  Int

  END INTERFACE

  PROCEDURE(Int),    POINTER :: PtrInt

  PtrInt => Int
  IF ( PtrInt(100) .NE. 100 ) STOP 11

! IF ( Int(1_1)    .NE. 1_1)  STOP 12

! IF ( Int(2_2)    .NE. 2_2)  STOP 13

  IF ( Int(8_8)    .NE. 8_8)  STOP 14


  END SUBROUTINE

  END

