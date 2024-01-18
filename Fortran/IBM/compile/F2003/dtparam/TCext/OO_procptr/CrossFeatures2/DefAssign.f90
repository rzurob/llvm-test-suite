! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures2/DefAssign.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : DefAssign.f
!*
!*  DATE                       : Jun. 23, 2005
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
!*  Defined assignment - it is incorrect to use procptr in defined assgn
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id=0
    CONTAINS
      PROCEDURE, PASS :: Proc => ModFun
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    CLASS(DT(*,4)) :: Arg
    TYPE(DT(20,4))  :: ModFun
      ModFun = Arg
    END FUNCTION

    SUBROUTINE MyAssign1 (Arg1, Arg2)
    TYPE(DT(*,4)), INTENT(OUT) :: Arg1
    PROCEDURE(ModFun), POINTER, INTENT(IN) :: Arg2
      Arg1 = Arg2(Arg1)
    END SUBROUTINE

  END MODULE


  PROGRAM DefAssign
  USE M
  IMPLICIT NONE

  PROCEDURE(ModFun), POINTER :: ProcPtr
  TYPE(DT(20,4)),          TARGET  :: U

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE MyAssign1
    END INTERFACE ASSIGNMENT ( = )

  U = ProcPtr

  U = RetPtr(ModFun)

  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(ModFun), POINTER :: RetPtr
  PROCEDURE(ModFun) :: Arg
    RetPtr => Arg
  END FUNCTION

  END

