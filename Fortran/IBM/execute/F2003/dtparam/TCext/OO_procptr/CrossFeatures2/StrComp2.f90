! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_procptr/CrossFeatures2/StrComp2.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: StrComp2.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : StrComp2.f
!*
!*  DATE                       : Jun. 24, 2005
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
!*  Structure component - parent component
!*  (315447)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1=>NULL()
    END TYPE

    TYPE, EXTENDS(Base)  :: DT    ! (4)
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2=>NULL()
      TYPE(Base(K1)) :: BComp
    END TYPE

    CONTAINS

    FUNCTION ModFun1(Arg1, Arg2)
    CLASS(Base(4)) :: Arg1
    REAL(4) :: ModFun1, Arg2
      ModFun1 = Arg2
    END FUNCTION

    FUNCTION ModFun2(Arg1, Arg2)
    CLASS(DT(4)) :: Arg1
    REAL(8) :: ModFun2, Arg2
      ModFun2 = Arg2
    END FUNCTION

  END MODULE

  PROGRAM StrComp2
  USE M
  IMPLICIT NONE

  TYPE(DT(4)) :: V=DT(4)(Base=Base(4)(NULL()), ProcPtr2=NULL(), BComp=Base(4)(NULL()))

  V = DT(4)(Base=Base(4)(ModFun1), ProcPtr2=ModFun2, BComp=Base(4)(ModFun1))

  IF ( .NOT. ASSOCIATED(V%ProcPtr1))       STOP 11
  IF ( .NOT. ASSOCIATED(V%BComp%ProcPtr1)) STOP 12
  IF ( .NOT. ASSOCIATED(V%ProcPtr2))       STOP 13

  IF ( V%Base%ProcPtr1(4.0)  .NE. 4.0 )   STOP 21
  IF ( V%BComp%ProcPtr1(4.0) .NE. 4.0 )   STOP 22
  IF ( V%ProcPtr2(8.0_8)      .NE. 8.0_8 ) STOP 23

  END

