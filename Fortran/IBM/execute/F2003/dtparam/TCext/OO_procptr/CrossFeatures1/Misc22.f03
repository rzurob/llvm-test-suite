! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures1/Misc22.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 14, 2005
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
!*  Procedure pointer component with implicit interface
!*
!*  (Failed: implicit interface problem)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
  LOGICAL L

  CONTAINS

  FUNCTION F()
  INTEGER F
    L = .TRUE.
    F = 1
  END FUNCTION

  SUBROUTINE S()
    L = .TRUE.
    PRINT*, "In Sub"
  END SUBROUTINE

  END MODULE

  PROGRAM Misc22
  USE M
  IMPLICIT INTEGER(P)

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr
  END TYPE

  TYPE :: DT1(K2,N2)    ! (4,20)
    INTEGER, KIND :: K2
    INTEGER, LEN  :: N2
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  END TYPE


  TYPE(DT(4,20))  :: V
  TYPE(DT1(4,20)) :: U

  L = .FALSE.
  V%PRocPtr => F
  PRINT *, V%ProcPtr()
  IF( .NOT. L) ERROR STOP 11

  L = .FALSE.
  U%ProcPtr => S
  CALL U%ProcPtr()
  IF( .NOT. L) ERROR STOP 12

END

