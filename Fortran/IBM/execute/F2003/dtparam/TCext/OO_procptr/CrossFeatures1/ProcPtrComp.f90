! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures1/ProcPtrComp.f
! opt variations: -qnok -qnol

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
!*  Procedure pointer components
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


  PROGRAM ProcPtrComp
  USE M
  IMPLICIT NONE
  PROCEDURE(Int2), POINTER :: ProcInt2

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    PROCEDURE (INTEGER(1)), NOPASS, POINTER :: PtrInt1
    PROCEDURE (INTEGER(2)), NOPASS, POINTER :: PtrInt2
    PROCEDURE (INTEGER(4)), NOPASS, POINTER :: PtrInt4
    PROCEDURE (INTEGER(8)), NOPASS, POINTER :: PtrInt8
  END TYPE

  TYPE (DT(4,20)) :: W


  ProcInt2 => Int2
  CALL IntSub(Int1, ProcInt2, W )

  IF ( W%PtrInt1(2_1) .NE. 1_1 ) STOP 22
  IF ( W%PtrInt2(0_2) .NE. 2_2 ) STOP 23
  IF ( W%PtrInt4(-4)  .NE. -4 )  STOP 24
  IF ( W%PtrInt8(1_8) .NE. 8_8 ) STOP 25

  CONTAINS

  SUBROUTINE  IntSub(Proc, ProcPtr, Var)
  PROCEDURE(Int1)          :: Proc
  PROCEDURE(Int2), POINTER :: ProcPtr

  INTERFACE
    FUNCTION Int(Arg)
      INTEGER :: Int, Arg
    END FUNCTION
  END INTERFACE

  TYPE (DT(4,*)), INTENT(INOUT) :: Var
  TYPE (DT(4,20)), SAVE          :: V

  V%PtrInt1 => Proc
  IF ( V%PtrInt1(0_1) .NE. 1_1 ) STOP 11

  V%PtrInt1 => V%PtrInt1
  IF ( V%PtrInt1(2_1) .NE. 1_1 ) STOP 12

  V%PtrInt2 => ProcPtr
  IF ( V%PtrInt2(0_2) .NE. 2_2 ) STOP 13

  V%PtrInt4 => Int
  IF ( V%PtrInt4(-4)  .NE. -4 )  STOP 14

  V%PtrInt8 => Int8
  IF ( V%PtrInt8(1_8) .NE. 8_8 ) STOP 15

  Var = V

  END SUBROUTINE

  END

