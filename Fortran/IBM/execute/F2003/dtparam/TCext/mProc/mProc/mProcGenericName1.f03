! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mProc/mProc/mProcGenericName1.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar 03, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Generaliztion of PROCEDURE statement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 296676
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  A generic name specifies a single name to reference all of the procedure names in
!*  the interface block.  A generic name may be the same as any one of the procedure names
!*  in the interface block, or the same as any accessible generic name.
!*
!*  -- Module procedure
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: ID
  END TYPE

  TYPE :: DT1(K2)    ! (4)
    INTEGER, KIND :: K2
    INTEGER(K2)   :: ID
  END TYPE

  TYPE :: DT2(K3)    ! (4)
    INTEGER, KIND :: K3
    INTEGER(K3)   :: ID
  END TYPE

  TYPE :: DT3(K4)    ! (4)
    INTEGER, KIND :: K4
    INTEGER(K4)   :: ID
  END TYPE

  PROCEDURE(ModFun1), POINTER  :: ProcPtr


  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT(4)), INTENT(IN)   :: Arg
  CLASS(DT(4)), ALLOCATABLE  :: ModFun
    ALLOCATE(ModFun, SOURCE=Arg)
  END FUNCTION

  FUNCTION ModFun1(Arg)
  CLASS(DT1(4)), INTENT(IN) :: Arg
  CLASS(DT1(4)), POINTER    :: ModFun1
    ALLOCATE(ModFun1, SOURCE=Arg)
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M, ONLY: DT3
  CLASS(DT3(4)), INTENT(IN) :: Arg
  CLASS(DT3(4)), POINTER    :: ExtFun
    ALLOCATE(ExtFun, SOURCE=Arg)
  END FUNCTION


  PROGRAM mProcGenericName1
  USE M

  INTERFACE  ModFun
    FUNCTION ExtFun(Arg)
      IMPORT
      CLASS(DT3(4)), INTENT(IN)    :: Arg
      CLASS(DT3(4)), POINTER       :: ExtFun
    END FUNCTION
    PROCEDURE ExtFun
    PROCEDURE ModFun
    PROCEDURE ProcPtr
  END INTERFACE

  TYPE(DT(4))  :: T=DT(4)(-1)
  TYPE(DT1(4)) :: T1=DT1(4)(1)
  TYPE(DT2(4)) :: T2=DT2(4)(2)
  TYPE(DT3(4)) :: T3=DT3(4)(3)

  ProcPtr => ModFun1

  SELECT TYPE ( As => ModFun(DT(4)(-1)) )
  TYPE IS (DT(4))
    IF (As%ID   .NE. -1 ) ERROR STOP 11
  CLASS DEFAULT
    STOP 12
  END SELECT

  SELECT TYPE ( As => ModFun(DT1(4)(-2)) )
  TYPE IS (DT1(4))
    IF (As%ID   .NE. -2 ) ERROR STOP 11
  CLASS DEFAULT
    STOP 12
  END SELECT

  SELECT TYPE ( As => ModFun(DT3(4)(-3)) )
  TYPE IS (DT3(4))
    IF (As%ID   .NE. -3 ) ERROR STOP 11
  CLASS DEFAULT
    STOP 12
  END SELECT


  END

