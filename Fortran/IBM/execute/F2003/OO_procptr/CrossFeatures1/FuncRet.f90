! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: FuncRet.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncRet.f
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

  CONTAINS

    FUNCTION Int1(Arr)
    INTEGER(1) :: Arr(:)
    INTEGER(1) :: Int1(SIZE(Arr))
      Int1 = Arr
    END FUNCTION

    FUNCTION Int2(Arr)
    INTEGER(2) :: Arr(:)
    INTEGER(2) :: Int2(SIZE(Arr))
      Int2 = Arr
    END FUNCTION

    FUNCTION Int4(Arr)
    INTEGER(4) :: Arr(:)
    INTEGER(4) :: Int4(SIZE(Arr))
      Int4 = Arr
    END FUNCTION

    FUNCTION Int8(Arr)
    INTEGER(8) :: Arr(:)
    INTEGER(8) :: Int8(SIZE(Arr))
      Int8 = Arr
    END FUNCTION

    FUNCTION REAL4(Arr)
    REAL(4) :: Arr(:)
    REAL(4) :: REAL4(SIZE(Arr))
      REAL4 = Arr
    END FUNCTION

    FUNCTION REAL8(Arr)
    REAL(8) :: Arr(:)
    REAL(8) :: REAL8(SIZE(Arr))
      REAL8 = Arr
    END FUNCTION

    FUNCTION REAL16(Arr)
    REAL(16) :: Arr(:)
    REAL(16) :: REAL16(SIZE(Arr))
      REAL16 = Arr
    END FUNCTION

    FUNCTION Complex8(Arr)
    COMPLEX(8) :: Arr(:)
    COMPLEX(8) :: Complex8(SIZE(Arr))
      Complex8 = Arr
    END FUNCTION

    FUNCTION Complex16(Arr)
    COMPLEX(16) :: Arr(:)
    COMPLEX(16) :: Complex16(SIZE(Arr))
      Complex16 = Arr
    END FUNCTION

    FUNCTION Log1(Arr)
    LOGICAL(1) :: Arr(:)
    LOGICAL(1) :: Log1(SIZE(Arr))
      Log1 = Arr
    END FUNCTION

    FUNCTION Log2(Arr)
    LOGICAL(2) :: Arr(:)
    LOGICAL(2) :: Log2(SIZE(Arr))
      Log2 = Arr
    END FUNCTION

    FUNCTION Log4(Arr)
    LOGICAL(4) :: Arr(:)
    LOGICAL(4) :: Log4(SIZE(Arr))
      Log4 = Arr
    END FUNCTION

    FUNCTION Log8(Arr)
    LOGICAL(8) :: Arr(:)
    LOGICAL(8) :: Log8(SIZE(Arr))
      Log8 = Arr
    END FUNCTION




  END MODULE


  PROGRAM FuncRet
  USE M

  INTERFACE
    FUNCTION I1(Arr)
    INTEGER(1) :: Arr(:)
    INTEGER(1) :: I1(SIZE(Arr))
    END FUNCTION

    FUNCTION I2(Arr)
    INTEGER(2) :: Arr(:)
    INTEGER(2) :: I2(SIZE(Arr))
    END FUNCTION

    FUNCTION I4(Arr)
    INTEGER(4) :: Arr(:)
    INTEGER(4) :: I4(SIZE(Arr))
    END FUNCTION

    FUNCTION I8(Arr)
    INTEGER(8) :: Arr(:)
    INTEGER(8) :: I8(SIZE(Arr))
    END FUNCTION

    FUNCTION R4(Arr)
    REAL(4) :: Arr(:)
    REAL(4) :: R4(SIZE(Arr))
    END FUNCTION

    FUNCTION R8(Arr)
    REAL(8) :: Arr(:)
    REAL(8) :: R8(SIZE(Arr))
    END FUNCTION

    FUNCTION R16(Arr)
    REAL(16) :: Arr(:)
    REAL(16) :: R16(SIZE(Arr))
    END FUNCTION

    FUNCTION C8(Arr)
    COMPLEX(8) :: Arr(:)
    COMPLEX(8) :: C8(SIZE(Arr))
    END FUNCTION

    FUNCTION C16(Arr)
    COMPLEX(16) :: Arr(:)
    COMPLEX(16) :: C16(SIZE(Arr))
    END FUNCTION

    FUNCTION L1(Arr)
    LOGICAL(1) :: Arr(:)
    LOGICAL(1) :: L1(SIZE(Arr))
    END FUNCTION

    FUNCTION L2(Arr)
    LOGICAL(2) :: Arr(:)
    LOGICAL(2) :: L2(SIZE(Arr))
    END FUNCTION

    FUNCTION L4(Arr)
    LOGICAL(4) :: Arr(:)
    LOGICAL(4) :: L4(SIZE(Arr))
    END FUNCTION

    FUNCTION L8(Arr)
    LOGICAL(8) :: Arr(:)
    LOGICAL(8) :: L8(SIZE(Arr))
    END FUNCTION

  END INTERFACE

  PROCEDURE(I1), POINTER :: ProcPtrI1
  PROCEDURE(I2), POINTER :: ProcPtrI2
  PROCEDURE(I4), POINTER :: ProcPtrI4
  PROCEDURE(I8), POINTER :: ProcPtrI8

  PROCEDURE(R4),  POINTER :: ProcPtrR4
  PROCEDURE(R8),  POINTER :: ProcPtrR8
  PROCEDURE(R16), POINTER :: ProcPtrR16

  PROCEDURE(C8),  POINTER :: ProcPtrC8
  PROCEDURE(C16), POINTER :: ProcPtrC16

  PROCEDURE(L1),  POINTER :: ProcPtrL1
  PROCEDURE(L2),  POINTER :: ProcPtrL2
  PROCEDURE(L4),  POINTER :: ProcPtrL4
  PROCEDURE(L8),  POINTER :: ProcPtrL8



  ProcPtrI1 => Int1
  IF ( ANY(ProcPtrI1((/-1_1/)) .NE. (/-1_1/) )) STOP 11

  ProcPtrI2 => Int2
  IF ( ANY(ProcPtrI2((/-2_2/)) .NE. (/-2_2/) )) STOP 12

  ProcPtrI4 => Int4
  IF ( ANY(ProcPtrI4((/-4_4/)) .NE. (/-4_4/) )) STOP 14

  ProcPtrI8 => Int8
  IF ( ANY(ProcPtrI8((/-8_8/)) .NE. (/-8_8/) )) STOP 18

  ProcPtrR4 => Real4
  IF ( ANY(ProcPtrR4((/-4.0_4/)) .NE. (/-4.0_4/) )) STOP 24

  ProcPtrR8 => Real8
  IF ( ANY(ProcPtrR8((/-8.0_8/)) .NE. (/-8.0_8/) )) STOP 28

  ProcPtrR16 => Real16
  IF ( ANY(ProcPtrR16((/-16.0_16/)) .NE. (/-16.0_16/) )) STOP 216

  ProcPtrC8 => Complex8
  IF ( ANY(ProcPtrC8((/(8.0_8, -8.0_8)/)) .NE. (/(8.0_8, -8.0_8)/) )) STOP 38

  ProcPtrC16 => Complex16
  IF ( ANY(ProcPtrC16((/(16.0_16, -16.0_16)/)) .NE. (/(16.0_16, -16.0_16)/) )) STOP 316

  ProcPtrL1 => Log1
  IF ( ANY(ProcPtrL1((/.TRUE._1/)) .NEQV. (/.TRUE._1/) )) STOP 41

  ProcPtrL2 => Log2
  IF ( ANY(ProcPtrL2((/.TRUE._2/)) .NEQV. (/.TRUE._2/) )) STOP 42

  ProcPtrL4 => Log4
  IF ( ANY(ProcPtrL4((/.FALSE._4/)) .NEQV. (/.FALSE._4/) )) STOP 44

  ProcPtrL8 => Log8
  IF ( ANY(ProcPtrL8((/.FALSE._8/)) .NEQV. (/.FALSE._8/) )) STOP 48


  END

