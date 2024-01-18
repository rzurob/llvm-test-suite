! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/OO_procptr/CrossFeatures1/Arg5.f
! opt variations: -qnok -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 20, 2005
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
!*  Dummy procedure -
!*                     a function returning a procedure pointer
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      SEQUENCE
      PROCEDURE(), POINTER, NOPASS :: ProcPtr
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base(4)) :: Arg, IntF
      END FUNCTION
    END INTERFACE

    LOGICAL :: OK=.FALSE.

    CONTAINS

    SUBROUTINE ModSub()
      OK = .TRUE.
      print*, "ok"
    END SUBROUTINE

    FUNCTION RetPtr(Arg)
    PROCEDURE(IntF) :: Arg
    PROCEDURE(IntF), POINTER :: RetPtr
      RetPtr => Arg
    END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(4)) :: Arg, ExtFun
    ExtFun = Arg
  END FUNCTION

  PROGRAM Arg5
  USE M
  IMPLICIT NONE
  PROCEDURE(IntF)          :: ExtFun
  PROCEDURE(IntF), POINTER :: ProcPtr
  TYPE(Base(4))               :: V

  ProcPtr => RetPtr(ExtFun)
  V = ProcPtr(Base(4)(ModSub))
  IF (.NOT. ASSOCIATED(V%ProcPtr, ModSub) ) STOP 11
  CALL V%ProcPtr()
  IF ( .NOT. OK )  STOP 12

  END


