! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/mProc/mProc/mProcGenericName4.f
! opt variations: -qnol

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
!*  -- Entry
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    INTEGER(K1)   :: ID
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(*), INTENT(IN) :: Arg(:), Arg1, Arg2
  CLASS(*), POINTER    :: ModFun(:)
  CLASS(*), POINTER    :: ModFun1(:)
  CLASS(*), POINTER    :: ModFun2(:)

    ALLOCATE(ModFun(SIZE(Arg)), SOURCE=Arg)
    RETURN

  ENTRY  ModFun1(Arg, Arg1)
    ALLOCATE(ModFun1(SIZE(Arg)), SOURCE=Arg)
    RETURN

  Entry ModFun2(Arg, Arg1, Arg2)
    ALLOCATE(ModFun2(SIZE(Arg)), SOURCE=Arg)
    RETURN

  END FUNCTION

  END MODULE

  PROGRAM mProcGenericName4
  USE M

  INTERFACE  ModFun1
    PROCEDURE ModFun
    PROCEDURE ModFun1
    MODULE PROCEDURE ModFun2
  END INTERFACE


  SELECT TYPE ( As => ModFun1((/DT(20,4)(-1)/)) )
  TYPE IS (DT(*,4))
    IF (SIZE(As) .NE.  1 ) ERROR STOP 11
    IF (As(1)%ID .NE. -1 ) ERROR STOP 12
  CLASS DEFAULT
    STOP 13
  END SELECT

  SELECT TYPE ( As => ModFun1((/DT(20,4)(-2), DT(20,4)(-3)/), DT(20,4)(-2)) )
  TYPE IS (DT(*,4))
    IF (SIZE(As) .NE.  2 )          ERROR STOP 21
    IF (ANY( As%ID.NE. (/-2,-3/)) ) ERROR STOP 22
  CLASS DEFAULT
    STOP 23
  END SELECT

  SELECT TYPE ( As => ModFun1((/DT(20,4)(1),DT(20,4)(2),DT(20,4)(3)/),DT(20,4)(-3),DT(20,4)(-3)) )
  TYPE IS (DT(*,4))
    IF (SIZE(As) .NE.  3 )          ERROR STOP 31
    IF (ANY( As%ID.NE. (/1,2,3/)) ) ERROR STOP 32
  CLASS DEFAULT
    STOP 33
  END SELECT

  END

