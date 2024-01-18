! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mProc/mProc/mProcGenericName3.f
! opt variations: -ql

!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : mProcGenericName3.f
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
!*  -- Dummy procedure
!*  (316925)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(K1)    ! (4)
    INTEGER, KIND :: K1
    INTEGER(K1)   :: ID
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(*), INTENT(IN)   :: Arg(:)
  CLASS(*), ALLOCATABLE  :: ModFun(:)
    ALLOCATE(ModFun(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION

  FUNCTION ModFun1(Arg, Arg1)
  CLASS(*), INTENT(IN) :: Arg(:), Arg1
  CLASS(*), POINTER    :: ModFun1(:)
    ALLOCATE(ModFun1(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg, Arg1, Arg2)
  USE M
  CLASS(*), INTENT(IN) :: Arg(:), Arg1, Arg2
  CLASS(*), POINTER    :: ExtFun(:)
    ALLOCATE(ExtFun(SIZE(Arg)), SOURCE=Arg)
  END FUNCTION


  PROGRAM mProcGenericName3
  USE M

  INTERFACE  Proc
    FUNCTION ExtFun(Arg, Arg1, Arg2)
      IMPORT
      CLASS(*), INTENT(IN)    :: Arg(:), Arg1, Arg2
      CLASS(*), POINTER       :: ExtFun(:)
    END FUNCTION
    PROCEDURE ExtFun
  END INTERFACE

  CONTAINS

  SUBROUTINE Sub(Proc)
  PROCEDURE(ModFun1) Proc

  INTERFACE  Proc
    PROCEDURE Proc
    PROCEDURE ModFun
  END INTERFACE

  SELECT TYPE ( As => Proc((/DT(4)(-1)/)) )
  TYPE IS (DT(4))
    IF (SIZE(As) .NE.  1 ) STOP 11
    IF (As(1)%ID .NE. -1 ) STOP 12
  CLASS DEFAULT
    STOP 13
  END SELECT

  SELECT TYPE ( As => Proc((/DT(4)(-2), DT(4)(-3)/), DT(4)(-2)) )
  TYPE IS (DT(4))
    IF (SIZE(As) .NE.  2 )          STOP 21
    IF (ANY( As%ID.NE. (/-2,-3/)) ) STOP 22
  CLASS DEFAULT
    STOP 23
  END SELECT

  SELECT TYPE ( As => Proc((/DT(4)(1),DT(4)(2),DT(4)(3)/),DT(4)(-3),DT(4)(-3)) )
  TYPE IS (DT(4))
    IF (SIZE(As) .NE.  3 )          STOP 31
    IF (ANY( As%ID.NE. (/1,2,3/)) ) STOP 32
  CLASS DEFAULT
    STOP 33
  END SELECT

  END SUBROUTINE

  END

