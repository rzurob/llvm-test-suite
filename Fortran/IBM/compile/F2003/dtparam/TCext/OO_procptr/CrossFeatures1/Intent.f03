! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures1/Intent.f
! opt variations: -qck -qnok

! *********************************************************************
!*  ===================================================================
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
!*  Intent - pointer assignment and nullify
!*
!*  (304243/ICE)
!*  (304645)
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT TYPE(Base(4,3))(P)

    TYPE :: Base(K1,N1)    ! (4,3)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      CHARACTER(N1) :: C
    END TYPE

    INTERFACE
      SUBROUTINE IntF(Arg1, Arg2)
      IMPORT
        TYPE(Base(4,*)), INTENT(IN)  :: Arg2
        TYPE(Base(4,*)), INTENT(OUT) :: Arg1
      END SUBROUTINE
    END INTERFACE
  END MODULE


  PROGRAM Intent
  USE M
  IMPLICIT NONE
  PROCEDURE(IntF)          :: ExtSub
  PROCEDURE(IntF), POINTER :: ProcPtr

  ProcPtr => ExtSub
  CALL ModSub1(ProcPtr, ExtSub)
  CALL ModSub2(ProcPtr, ExtSub)
  CALL ModSub3(ProcPtr, ExtSub)


  CONTAINS

    SUBROUTINE ModSub1(ProcPtr, Proc)
    PROCEDURE(IntF), POINTER, INTENT(IN) :: ProcPtr
    PROCEDURE(IntF)                      :: Proc
      ProcPtr => Proc
    END SUBROUTINE

    SUBROUTINE ModSub2(ProcPtr, Proc)
    PROCEDURE(), POINTER, INTENT(IN) :: ProcPtr
    PROCEDURE()                  :: Proc
      NULLIFY(ProcPtr)
    END SUBROUTINE

    SUBROUTINE ModSub3(ProcPtr, Proc)
    PROCEDURE(), POINTER, INTENT(IN) :: ProcPtr
    PROCEDURE(IntF)                  :: Proc
      NULLIFY(ProcPtr)
      ProcPtr => Proc
    END SUBROUTINE

  END

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(Base(4,*)), INTENT(IN)  :: Arg2
  TYPE(Base(4,*)), INTENT(OUT) :: Arg1
  END SUBROUTINE
