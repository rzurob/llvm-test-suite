! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 28, 2005
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
!*  ASSOCIATED(POINTER [, TARGET])
!*
!*  (Comp failed)
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


  PROGRAM TheAssociatedIntrinsic
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

    MODULE PROCEDURE  Int8

  END INTERFACE

! Not implemented for the procedure stmt in interface
! INTERFACE Int
!          PROCEDURE  Proc
!          PROCEDURE  ProcPtr
!   MODULE PROCEDURE  Int8
!          PROCEDURE  Int
! END INTERFACE

  PROCEDURE(Int),         POINTER :: PtrInt  => NULL()
  PROCEDURE(INTEGER(1)),  POINTER :: PtrInt1 => NULL()
  PROCEDURE(INTEGER(2)),  POINTER :: PtrInt2 => NULL()
  PROCEDURE(INTEGER(4)),  POINTER :: PtrInt4 => NULL()
  PROCEDURE(INTEGER(8)),  POINTER :: PtrInt8 => NULL()


  IF ( ASSOCIATED( PtrInt ))            STOP 11
  PtrInt => Int
  IF ( .NOT. ASSOCIATED( PtrInt, Int )) STOP 12
  IF ( PtrInt(100) .NE. 100 )           STOP 13

  IF ( ASSOCIATED( PtrInt1 ))           STOP 21
  PtrInt1 => Proc
  IF ( .NOT. ASSOCIATED( PtrInt1, Int1)) STOP 22
  IF ( PtrInt1(1_1) .NE. 1_1)           STOP 23

  IF ( ASSOCIATED( PtrInt2 ))           STOP 31
  PtrInt2 => ProcPtr
  IF ( .NOT. ASSOCIATED(PtrInt2, Int2)) STOP 32
  IF ( PtrInt2(3_2) .NE. 2_2)           STOP 33

  IF ( ASSOCIATED( PtrInt4 ))           STOP 41
  PtrInt4 => PtrInt
  IF ( .NOT. ASSOCIATED(PtrInt4, Int) ) STOP 42
  IF ( PtrInt4(3) .NE. 3)               STOP 43

  IF ( ASSOCIATED( PtrInt8 ))           STOP 51
  PtrInt8 => Int8
  IF ( .NOT. ASSOCIATED(PtrInt8, Int8)) STOP 52
  IF ( PtrInt8(3_8) .NE. 8_8)           STOP 53

  END SUBROUTINE

  END

