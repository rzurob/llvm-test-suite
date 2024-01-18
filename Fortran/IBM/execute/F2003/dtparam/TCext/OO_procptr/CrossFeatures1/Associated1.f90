! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/CrossFeatures1/Associated1.f
! opt variations: -qnok -ql

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
!*  on proc pointer components
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    CONTAINS

    FUNCTION Char0(Arg)
    CHARACTER(0) :: Char0, Arg
      Char0 = ""  ! nothing happens
    END FUNCTION

    FUNCTION Char1(Arg)
    CHARACTER(1) :: Char1, Arg
      Char1 = "1"
    END FUNCTION

    FUNCTION Char2(Arg)
    CHARACTER(2) :: Char2, Arg
      Char2 = "2"
    END FUNCTION

    FUNCTION Char3(Arg)
    CHARACTER(1025) :: Char3, Arg
      Char3 = "1025"
    END FUNCTION

  END MODULE

  FUNCTION Char4(Arg)
  CHARACTER(511) :: Char4, Arg
    Char4 = "511"
  END FUNCTION


  PROGRAM Associated1
  USE M
  IMPLICIT NONE
  PROCEDURE(CHARACTER(2)), POINTER :: ProcPtrChar2


  ProcPtrChar2 => Char2
  CALL IntSub(Char1, ProcPtrChar2 )

  CONTAINS

  SUBROUTINE  IntSub(Proc, ProcPtr)
  PROCEDURE(CHARACTER(1))          :: Proc
  PROCEDURE(CHARACTER(2)), POINTER :: ProcPtr

  INTERFACE
    FUNCTION Char4(Arg)
    CHARACTER(511) :: Char4, Arg
    END FUNCTION
  END INTERFACE

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    PROCEDURE(CHARACTER(0)),    POINTER, NOPASS :: PtrChar0  => NULL()
    PROCEDURE(CHARACTER(1)),    POINTER, NOPASS :: PtrChar1  => NULL()
    PROCEDURE(CHARACTER(2)),    POINTER, NOPASS :: PtrChar2  => NULL()
    PROCEDURE(CHARACTER(1025)), POINTER, NOPASS :: PtrChar3  => NULL()
    PROCEDURE(CHARACTER(511)),  POINTER, NOPASS :: PtrChar4  => NULL()
  END TYPE

  TYPE (DT(4)) :: V

  IF ( ASSOCIATED( V%PtrChar0 ))           STOP 10
  IF ( ASSOCIATED( V%PtrChar1 ))           STOP 11
  IF ( ASSOCIATED( V%PtrChar2 ))           STOP 12
  IF ( ASSOCIATED( V%PtrChar3 ))           STOP 13
  IF ( ASSOCIATED( V%PtrChar4 ))           STOP 14

  V = DT(4)(Char0, Proc, ProcPtr, Char3, Char4 )

  IF ( .NOT. ASSOCIATED( V%PtrChar0 ))     STOP 20
  IF ( .NOT. ASSOCIATED( V%PtrChar1 ))     STOP 21
  IF ( .NOT. ASSOCIATED( V%PtrChar2 ))     STOP 22
  IF ( .NOT. ASSOCIATED( V%PtrChar3 ))     STOP 23
  IF ( .NOT. ASSOCIATED( V%PtrChar4 ))     STOP 24


  IF ( LEN( V%PtrChar0(""))      .NE. 0 )      STOP 30
  IF ( V%PtrChar1("1")           .NE. "1" )    STOP 31
  IF ( V%PtrChar2("2")           .NE. "2" )    STOP 32
  IF ( TRIM(V%PtrChar3("long"))  .NE. "1025" ) STOP 33
  IF ( TRIM(V%PtrChar4("short")) .NE. "511" )  STOP 34


  END SUBROUTINE

  END

