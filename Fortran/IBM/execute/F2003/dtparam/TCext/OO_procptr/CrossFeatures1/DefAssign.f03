! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp /tstdev/OO_procptr/CrossFeatures1/DefAssign.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 17, 2005
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
!*  Defined assignment
!*  (304718) -> 305334
!*   NOTE from JX: proc-ptr not allowed in defined assgn; test case updated
!(2008-06-11)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION CToC(Arg)
       CHARACTER(*) :: Arg
       CHARACTER(LEN(Arg)) :: CToC
      END FUNCTION
    END INTERFACE

    TYPE :: Base(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
      PROCEDURE(CToC), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE :: DT(N2,K2)    ! (20,4)
      INTEGER, KIND             :: K2
      INTEGER, LEN              :: N2
      INTEGER(K2)               :: Id=0
      TYPE(Base(K2,:)), POINTER :: BComp
    END TYPE

    INTERFACE ASSGN
      MODULE PROCEDURE MyAssign1
!     MODULE PROCEDURE MyAssign2
      MODULE PROCEDURE MyAssign3
    END INTERFACE

    CONTAINS

    FUNCTION Fun(Arg)
    CHARACTER(*) :: Arg
    CHARACTER(LEN(Arg)) :: Fun
      Fun = Arg
    END FUNCTION

    SUBROUTINE MyAssign1 (Arg1, Arg2)
    PROCEDURE(CToC), POINTER, INTENT(OUT) :: Arg1
    PROCEDURE(CToC), POINTER, INTENT(IN) :: Arg2
      Arg1 => Arg2
    END SUBROUTINE

!   SUBROUTINE MyAssign2 (Arg1, Arg2)
!   PROCEDURE(CToC), POINTER, INTENT(OUT) :: Arg1
!   PROCEDURE(CToC), INTENT(IN) :: Arg2 !disallowed by C1214
!     Arg1 => Arg2
!   END SUBROUTINE

    SUBROUTINE MyAssign3 (Arg1, Arg2)
    PROCEDURE(CToC), POINTER, INTENT(OUT) :: Arg1
    TYPE(DT(*,4)), INTENT(IN) :: Arg2
      Arg1 => Arg2%BComp%ProcPtr
    END SUBROUTINE

  END MODULE


  PROGRAM DefAssign
  USE M
  IMPLICIT NONE

  PROCEDURE(CToC), POINTER :: ProcPtr
  PROCEDURE(CToC), POINTER :: ProcPtr1
  TYPE(Base(4,20)),      TARGET  :: BTar

  ProcPtr => NULL()
!  ProcPtr = RetPtr(Fun)
  call assgn (ProcPtr, RetPtr(Fun))
  IF (ProcPtr("ABC") .NE. "ABC" ) ERROR STOP 14

  ProcPtr => Fun
!  ProcPtr = NULL(ProcPtr)
  call assgn (ProcPtr, NULL(ProcPtr))
  IF (ASSOCIATED(ProcPtr) ) ERROR STOP 15

  ProcPtr1 => Fun
!  ProcPtr = ProcPtr1
  call assgn (ProcPtr, ProcPtr1)
  IF (ProcPtr("0123456789") .NE. "0123456789" ) ERROR STOP 16

  ProcPtr => NULL()
  BTar =  Base(4,20)( Fun)
!  ProcPtr = DT(20,4)(-1, BTar)
  call assgn (ProcPtr, DT(20,4)(-1, BTar))
  IF (ProcPtr("xyz") .NE. "xyz" ) ERROR STOP 17

  CONTAINS

  FUNCTION RetPtr(Arg)
  PROCEDURE(CToC), POINTER :: RetPtr
  PROCEDURE(CToC) :: Arg
    RetPtr => Arg
  END FUNCTION

  END

