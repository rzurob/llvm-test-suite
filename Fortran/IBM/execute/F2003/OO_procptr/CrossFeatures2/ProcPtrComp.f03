! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 28, 2005
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
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE :: DT
      PROCEDURE (Int1), POINTER :: PtrInt1=>NULL()
      PROCEDURE (Int2), POINTER :: PtrInt2=>NULL()
      PROCEDURE (Int4), POINTER :: PtrInt4=>NULL()
      PROCEDURE (Int8), POINTER :: PtrInt8=>NULL()
    END TYPE

    TYPE (DT), SAVE :: W

    CONTAINS

    FUNCTION Int1(Arg)
    CLASS(DT)  :: Arg
    INTEGER(1) :: Int1
      Int1 = 1_1
    END FUNCTION

    FUNCTION Int2(Arg)
    CLASS(DT)  :: Arg
    INTEGER(2) :: Int2
      Int2 = 2_2
    END FUNCTION

    FUNCTION Int4(Arg)
    CLASS(DT)  :: Arg
    INTEGER :: Int4
      Int4 = 4_4
    END FUNCTION

    FUNCTION Int8(Arg)
    CLASS(DT)  :: Arg
    INTEGER(8) :: Int8
      Int8 = 8_8
    END FUNCTION

  END MODULE


  PROGRAM ProcPtrComp
  USE M
  IMPLICIT NONE
  PROCEDURE(Int2), POINTER :: ProcInt2

  ProcInt2 => Int2
  CALL IntSub(Int1, ProcInt2, W )

  IF ( W%PtrInt1() .NE. 1_1 ) ERROR STOP 22
  IF ( W%PtrInt2() .NE. 2_2 ) ERROR STOP 23
  IF ( W%PtrInt4()  .NE. 4 )  ERROR STOP 24
  IF ( W%PtrInt8() .NE. 8_8 ) ERROR STOP 25

  CONTAINS

  SUBROUTINE  IntSub(Proc, ProcInt, Var)
  PROCEDURE(Int1) :: Proc
  PROCEDURE(Int2) :: ProcInt
  TYPE (DT)       :: V, Var

  V%PtrInt1 => Proc
  IF ( V%PtrInt1() .NE. 1_1 ) ERROR STOP 11

  V%PtrInt1 => V%PtrInt1
  IF ( V%PtrInt1() .NE. 1_1 ) ERROR STOP 12

  V%PtrInt2 => ProcInt
  IF ( V%PtrInt2() .NE. 2_2 ) ERROR STOP 13

  V%PtrInt4 => Int4
  IF ( V%PtrInt4()  .NE. 4 )  ERROR STOP 14

  V%PtrInt8 => Int8
  IF ( V%PtrInt8() .NE. 8_8 )  ERROR STOP 15

  Var = V

  END SUBROUTINE

  END
