! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/selectType/CrossFeatures/ProcInterface4.f
! opt variations: -qnol -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 02, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!* Procedure Interface
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M
    TYPE  :: DT0(N1,K1)    ! (20,8)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: I0=0
      CONTAINS
      PROCEDURE, PASS   :: GetInt
    END TYPE

    TYPE,  EXTENDS(DT0) :: DT1    ! (20,8)
      INTEGER(K1) :: I1=1
    END TYPE

    TYPE, EXTENDS(DT1) :: DT    ! (20,8)
      INTEGER(K1) :: I2=2
    END TYPE

    CONTAINS

    FUNCTION GetInt(Arg)
    CLASS(DT0(*,8)) :: Arg
    INTEGER(8), ALLOCATABLE :: GetInt
      SELECT TYPE (Arg)
      TYPE IS (DT0(*,8))
        ALLOCATE(GetInt, SOURCE=Arg%I0)
      TYPE IS (DT1(*,8))
        ALLOCATE(GetInt, SOURCE=Arg%I1)
      TYPE IS (DT(*,8))
        ALLOCATE(GetInt, SOURCE=Arg%I2)
      CLASS DEFAULT
        STOP 20
      END SELECT
    END FUNCTION

  END MODULE

  PROGRAM ProcInterface4
  USE M
  IMPLICIT CLASS(DT(20,8))(U)

  INTERFACE
    FUNCTION UFun(UArg)
      CLASS(*), POINTER :: UFun
      CLASS(*) :: UArg
    END FUNCTION
  END INTERFACE

  CALL Sub(UFun)

  CONTAINS

  SUBROUTINE Sub(Fun)

  PROCEDURE(Ufun) :: Fun

  SELECT TYPE ( V =>Fun(DT(20,8)()) )
  CLASS IS (DT(*,8))

    IF (V%I0 .NE. 0) ERROR STOP 30
    IF (V%I1 .NE. 1) ERROR STOP 31
    IF (V%I2 .NE. 2) ERROR STOP 32

    IF (V%DT0%GetInt() .NE. 0) ERROR STOP 40
    IF (V%DT1%GetInt() .NE. 1) ERROR STOP 41
    IF (V%GetInt()     .NE. 2) ERROR STOP 42

  CLASS DEFAULT
    STOP 50
  END SELECT

  END SUBROUTINE

  END

  FUNCTION UFun(UArg)
  IMPLICIT NONE
  CLASS(*), POINTER :: UFun
  CLASS(*) :: UArg
    ALLOCATE(UFun, SOURCE=UArg)
  END FUNCTION

