! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures2/Misc4.f
! opt variations: -qnok -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: tcomp Misc4.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc4.f
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
!*  Procedure pointer - Diag with type binding
!*  (315064)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: DT(K1,N1)    ! (4,20)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      PROCEDURE(ModFun), PASS, POINTER :: ProcPtr
      CONTAINS
      PROCEDURE, PASS :: Proc => ModFun
    END TYPE

  CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT(4,*)) :: Arg
    TYPE(DT(4,20))  :: ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Misc4
  USE M

  TYPE(DT(4,20)) :: V
  PROCEDURE(ModFun), POINTER :: ProcPtr

  ! This is ok
  TYPE :: DT1(K2,N2)    ! (4,20)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: N2
    PROCEDURE(ModFun), NOPASS, POINTER :: ProcPtr
  END TYPE
  TYPE(DT1(4,20)) :: U

  ! All the following are wrong

! no need to test it as it will show sytax errors
! TYPE :: DT2
!   PROCEDURE(V%ProcPtr), NOPASS, POINTER :: ProcPtr1
!   PROCEDURE(V%Proc),    NOPASS, POINTER :: ProcPtr2
! END TYPE

! PROCEDURE(V%ProcPtr), POINTER :: ProcPtr3
! PROCEDURE(V%Proc),    POINTER :: ProcPtr4

  ProcPtr => V%Proc

  ProcPtr => U%ProcPtr  ! this is ok

  END

