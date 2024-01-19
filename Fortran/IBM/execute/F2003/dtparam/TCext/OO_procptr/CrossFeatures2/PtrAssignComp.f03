! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/CrossFeatures2/PtrAssignComp.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 13, 2005
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
!*  Procedure pointer component
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  INTERFACE
    FUNCTION IntF(Arg)
    CLASS(*), ALLOCATABLE  :: IntF
    CLASS(*)               :: Arg
    END FUNCTION
  END INTERFACE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(*), ALLOCATABLE  :: ModFun
  CLASS(*)               :: Arg
    ALLOCATE(ModFun, SOURCE=Arg)
  END FUNCTION

  END MODULE

  PROGRAM  PtrAssign
  USE M, LModFun => ModFun, LIntF => IntF
  IMPLICIT NONE

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    PROCEDURE (LIntF), POINTER, NOPASS :: ProcPtr
  END TYPE

  CLASS(*), POINTER :: V

  ALLOCATE(V, SOURCE=DT(4)(LModFun))

  SELECT TYPE (V)
  TYPE IS (DT(4))

    V%ProcPtr => LModFun
    CALL IntSub( V%ProcPtr(10_1), 10_1 )

    V%ProcPtr => LModFun
    CALL IntSub( V%ProcPtr(1_1), 1_1 )

    CALL IntSub(V%ProcPtr(V%ProcPtr(1_1)), 1_1 )

    CALL IntSub( V%ProcPtr(-128_1),  -128_1 )

  CLASS DEFAULT
    STOP 55
  END SELECT

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  CLASS(*)    :: Arg1
  INTEGER(1)  :: Arg2

  SELECT TYPE( Arg1 )
  TYPE IS(INTEGER(1))
    IF (Arg1 .NE. Arg2) ERROR STOP 11
  CLASS DEFAULT
    STOP 12
  END SELECT

  END SUBROUTINE

  END

