! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 26, 2005
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
!*  FuncTion Return - intrinsic types
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    INTEGER(1), POINTER :: Int1(:)
    INTEGER(2), POINTER :: Int2(:)
    INTEGER(4), POINTER :: Int4(:)
    INTEGER(8), POINTER :: Int8(:)

    REAL(4),  POINTER :: REAL4(:)
    REAL(8),  POINTER :: REAL8(:)
    REAL(16), POINTER :: REAL16(:)

    COMPLEX(8),  POINTER :: Complex8(:)
    COMPLEX(16), POINTER :: Complex16(:)

    LOGICAL(1), POINTER :: Log1(:)
    LOGICAL(2), POINTER :: Log2(:)
    LOGICAL(4), POINTER :: Log4(:)
    LOGICAL(8), POINTER :: Log8(:)

  END TYPE

  CONTAINS

    FUNCTION Int1(Arr)
    INTEGER(1)             :: Arr(:)
    CLASS(DT), ALLOCATABLE :: Int1
      ALLOCATE(Int1)
      ALLOCATE(Int1%Int1(SIZE(Arr)))
      Int1%Int1 = Arr
    END FUNCTION

    FUNCTION Int2(Arr)
    INTEGER(2) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: Int2
      ALLOCATE(Int2)
      ALLOCATE(Int2%Int2(SIZE(Arr)))
      Int2%Int2 = Arr
    END FUNCTION

    FUNCTION Int4(Arr)
    INTEGER(4) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: Int4
      ALLOCATE(Int4)
      ALLOCATE(Int4%Int4(SIZE(Arr)))
      Int4%Int4 = Arr
    END FUNCTION

    FUNCTION Int8(Arr)
    INTEGER(8) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: Int8
      ALLOCATE(Int8)
      ALLOCATE(Int8%Int8(SIZE(Arr)))
      Int8%Int8 = Arr
    END FUNCTION

    FUNCTION REAL4(Arr)
    REAL(4) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: REAL4
      ALLOCATE(Real4)
      ALLOCATE(Real4%Real4(SIZE(Arr)))
      Real4%REAL4 = Arr
    END FUNCTION

    FUNCTION REAL8(Arr)
    REAL(8) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: REAL8
      ALLOCATE(Real8)
      ALLOCATE(Real8%Real8(SIZE(Arr)))
      Real8%REAL8 = Arr
    END FUNCTION

    FUNCTION REAL16(Arr)
    REAL(16) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: REAL16
      ALLOCATE(Real16)
      ALLOCATE(Real16%Real16(SIZE(Arr)))
      Real16%REAL16 = Arr
    END FUNCTION

    FUNCTION Complex8(Arr)
    COMPLEX(8) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: Complex8
      ALLOCATE(Complex8)
      ALLOCATE(Complex8%Complex8(SIZE(Arr)))
      Complex8%Complex8 = Arr
    END FUNCTION

    FUNCTION Complex16(Arr)
    COMPLEX(16) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: Complex16
      ALLOCATE(Complex16)
      ALLOCATE(Complex16%Complex16(SIZE(Arr)))
      Complex16%Complex16 = Arr
    END FUNCTION

    FUNCTION Log1(Arr)
    LOGICAL(1) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: Log1
      ALLOCATE(Log1)
      ALLOCATE(Log1%Log1(SIZE(Arr)))
      Log1%Log1 = Arr
    END FUNCTION

    FUNCTION Log2(Arr)
    LOGICAL(2) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: Log2
      ALLOCATE(Log2)
      ALLOCATE(Log2%Log2(SIZE(Arr)))
      Log2%Log2 = Arr
    END FUNCTION

    FUNCTION Log4(Arr)
    LOGICAL(4) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: Log4
      ALLOCATE(Log4)
      ALLOCATE(Log4%Log4(SIZE(Arr)))
      Log4%Log4 = Arr
    END FUNCTION

    FUNCTION Log8(Arr)
    LOGICAL(8) :: Arr(:)
    CLASS(DT), ALLOCATABLE :: Log8
      ALLOCATE(Log8)
      ALLOCATE(Log8%Log8(SIZE(Arr)))
      Log8%Log8 = Arr
    END FUNCTION

  END MODULE


  PROGRAM FuncRet
  USE M

  PROCEDURE(Int1), POINTER :: ProcPtrI1
  PROCEDURE(Int2), POINTER :: ProcPtrI2
  PROCEDURE(Int4), POINTER :: ProcPtrI4
  PROCEDURE(Int8), POINTER :: ProcPtrI8

  PROCEDURE(Real4),  POINTER :: ProcPtrR4
  PROCEDURE(Real8),  POINTER :: ProcPtrR8
  PROCEDURE(Real16), POINTER :: ProcPtrR16

  PROCEDURE(Complex8),  POINTER :: ProcPtrC8
  PROCEDURE(Complex16), POINTER :: ProcPtrC16

  PROCEDURE(Log1),  POINTER :: ProcPtrL1
  PROCEDURE(Log2),  POINTER :: ProcPtrL2
  PROCEDURE(Log4),  POINTER :: ProcPtrL4
  PROCEDURE(Log8),  POINTER :: ProcPtrL8

  TYPE(DT) :: V

  ProcPtrI1 => Int1
  V = ProcPtrI1((/-1_1/))
  IF ( ANY(V%Int1 .NE. (/-1_1/) )) ERROR STOP 11

  ProcPtrI2 => Int2
  V = ProcPtrI2((/-2_2/))
  IF ( ANY(V%Int2 .NE. (/-2_2/) )) ERROR STOP 12

  ProcPtrI4 => Int4
  V = ProcPtrI4((/-4_4/))
  IF ( ANY(V%Int4 .NE. (/-4_4/) )) ERROR STOP 14

  ProcPtrI8 => Int8
  V = ProcPtrI8((/-8_8/))
  IF ( ANY(V%Int8 .NE. (/-8_8/) )) ERROR STOP 18

  ProcPtrR4 => Real4
  V = ProcPtrR4((/-4.0_4/))
  IF ( ANY(V%Real4 .NE. (/-4.0_4/) )) ERROR STOP 24

  ProcPtrR8 => Real8
  V = ProcPtrR8((/-8.0_8/))
  IF ( ANY(V%Real8 .NE. (/-8.0_8/) )) ERROR STOP 28

  ProcPtrR16 => Real16
  V = ProcPtrR16((/-16.0_16/))
  IF ( ANY(V%Real16 .NE. (/-16.0_16/) )) ERROR STOP 216

  ProcPtrC8 => Complex8
  V = ProcPtrC8((/(8.0_8, -8.0_8)/))
  IF ( ANY(V%Complex8 .NE. (/(8.0_8, -8.0_8)/) )) ERROR STOP 38

  ProcPtrC16 => Complex16
  V = ProcPtrC16((/(16.0_16, -16.0_16)/))
  IF ( ANY(V%Complex16 .NE. (/(16.0_16, -16.0_16)/) )) ERROR STOP 316

  ProcPtrL1 => Log1
  V = ProcPtrL1((/.TRUE._1/))
  IF ( ANY(V%Log1 .NEQV. (/.TRUE._1/) )) ERROR STOP 41

  ProcPtrL2 => Log2
  V = ProcPtrL2((/.TRUE._2/))
  IF ( ANY(V%Log2 .NEQV. (/.TRUE._2/) )) ERROR STOP 42

  ProcPtrL4 => Log4
  V = ProcPtrL4((/.FALSE._4/))
  IF ( ANY(V%Log4 .NEQV. (/.FALSE._4/) )) ERROR STOP 44

  ProcPtrL8 => Log8
  V = ProcPtrL8((/.FALSE._8/))
  IF ( ANY(V%Log8 .NEQV. (/.FALSE._8/) )) ERROR STOP 48


  END
