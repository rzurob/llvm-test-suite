! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/mProc/mProc/mProcGenericName2.f
! opt variations: -qnol -qnodeferredlp

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
!*  -- Procedure pointer
!*  (316925/317692/318213)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  END TYPE

  PROCEDURE(ModFun1), POINTER  :: ProcPtr

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT(*,4)), INTENT(IN)   :: Arg
  CLASS(DT(:,4)), ALLOCATABLE  :: ModFun(:)
    ALLOCATE(ModFun(1), SOURCE=Arg)
  END FUNCTION

  FUNCTION ModFun1(Arg, Arg1)
  CLASS(DT(*,4)), INTENT(IN) :: Arg, Arg1
  CLASS(DT(:,4)), POINTER    :: ModFun1(:)
    ALLOCATE(ModFun1(1), SOURCE=Arg)
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg, Arg1, Arg2)
  USE M
  CLASS(DT(*,4)), INTENT(IN) :: Arg, Arg1, Arg2
  CLASS(DT(:,4)), POINTER    :: ExtFun(:)
    ALLOCATE(ExtFun(1), SOURCE=Arg)
  END FUNCTION


  PROGRAM mProcGenericName2
  USE M

  INTERFACE  ProcPtr
    FUNCTION ExtFun(Arg, Arg1, Arg2)
      IMPORT
      CLASS(DT(*,4)), INTENT(IN)    :: Arg , Arg1, Arg2
      CLASS(DT(:,4)), POINTER       :: ExtFun(:)
    END FUNCTION
    PROCEDURE ExtFun
    PROCEDURE ModFun
    PROCEDURE ProcPtr
  END INTERFACE

  ProcPtr => ModFun1

  SELECT TYPE ( As => ProcPtr(DT(20,4)(-1)) )
  TYPE IS (DT(*,4))
    IF (SIZE(As) .NE.  1 ) STOP 11
    IF (As(1)%ID .NE. -1 ) STOP 12
  CLASS DEFAULT
    STOP 13
  END SELECT

  SELECT TYPE ( As => ProcPtr(DT(20,4)(-2), DT(20,4)(-2)) )
  TYPE IS (DT(*,4))
    IF (SIZE(As) .NE.  1 ) STOP 11
    IF (As(1)%ID .NE. -2 ) STOP 12
  CLASS DEFAULT
    STOP 13
  END SELECT

  SELECT TYPE ( As => ProcPtr(DT(20,4)(-3),DT(20,4)(-3),DT(20,4)(-3)) )
  TYPE IS (DT(*,4))
    IF (SIZE(As) .NE.  1 ) STOP 11
    IF (As(1)%ID .NE. -3 ) STOP 12
  CLASS DEFAULT
    STOP 13
  END SELECT

  END

