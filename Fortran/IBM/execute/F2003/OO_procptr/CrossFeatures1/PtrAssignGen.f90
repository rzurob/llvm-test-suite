! *********************************************************************
!*  ===================================================================
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
!*  (304510)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    CONTAINS

    FUNCTION Int(Arg)
    INTEGER :: Int, Arg
      Int = Arg
    END FUNCTION

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

  MODULE M1
  USE M

    INTERFACE Int8
      MODULE PROCEDURE  Int
      MODULE PROCEDURE  Int1
      MODULE PROCEDURE  Int2
      MODULE PROCEDURE  Int8
    END INTERFACE

  END MODULE

  PROGRAM PtrAssignGen
  USE M
  USE M1
  IMPLICIT NONE

  INTERFACE Int
    MODULE PROCEDURE  Int
    MODULE PROCEDURE  Int1
    MODULE PROCEDURE  Int2
    MODULE PROCEDURE  Int8
  END INTERFACE

  PROCEDURE(Int),    POINTER :: PtrInt
  PROCEDURE(Int8),   POINTER :: PtrInt8

  PtrInt => Int
  IF ( PtrInt(100) .NE. 100 ) STOP 11

  PtrInt8 => Int8
  IF ( PtrInt8(100_8) .NE. 8_8 ) STOP 12

  END

