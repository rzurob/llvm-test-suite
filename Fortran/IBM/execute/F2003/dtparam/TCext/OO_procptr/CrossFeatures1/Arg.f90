! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures1/Arg.f
! with manual adjustment (decl procptr with explicit interface for param dt arg)
! opt variations: -qck -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 19, 2005
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
!*  Dummy argument is a procedure pointer - procedure pointer/function/Null
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE

  END MODULE

  MODULE M
  USE M0

    TYPE  :: DT(K2,N2)    ! (4,20)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      PROCEDURE(TYPE(Base(4,3))), NOPASS, POINTER :: ProcPtr
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg)
    TYPE(Base(4,*)) :: Arg
    TYPE(Base(4,3)) :: ModFun
      ModFun = Arg
    END FUNCTION

  END MODULE

  FUNCTION RetPtr(Fun)
  USE M
  PROCEDURE(ModFun)          :: Fun
  PROCEDURE(ModFun), POINTER :: RetPtr
    RetPtr => Fun
  END FUNCTION

  PROGRAM Arg
  USE M
  PROCEDURE(ModFun), POINTER :: ProcPtr

  INTERFACE
    FUNCTION RetPtr(Fun)
    IMPORT Base, ModFun
      PROCEDURE(ModFun)          :: Fun
      PROCEDURE(ModFun), POINTER :: RetPtr
    END FUNCTION
  END INTERFACE

  ProcPtr => ModFun
  CALL IntSub(ProcPtr)

  CONTAINS

  SUBROUTINE IntSub(Ptr)
  PROCEDURE(ModFun), POINTER :: Ptr
  TYPE(Base(4,3)) :: V
  TYPE(DT(4,20))   :: U, W
print *,'1'
  V = Ptr(Base(4,3)("123"))
print *,'2'
  IF ( V%C .NE. "123" ) ERROR STOP 12

print *,'3'
  U = DT(4,20)(RetPtr(Ptr))
print *,'4'
  IF ( .NOT. ASSOCIATED(U%ProcPtr, Ptr) ) ERROR STOP 32
print *,'5'

  END SUBROUTINE

  END

