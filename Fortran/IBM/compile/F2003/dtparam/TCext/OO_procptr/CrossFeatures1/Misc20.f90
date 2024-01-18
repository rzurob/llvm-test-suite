! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/OO_procptr/CrossFeatures1/Misc20.f
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
! %POSTCMD: tcomp Misc20.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  Misc20.f
!*
!*  DATE                       : Jun. 12, 2005
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
!*  Accessibility : Private/Public
!*
!*  (ICE-305108)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  Module M

  TYPE :: DT(K1,N1)    ! (4,20)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    SEQUENCE
    PROCEDURE(), POINTER, NOPASS, PRIVATE :: ProcPtr1
    PROCEDURE(), POINTER, NOPASS, PUBLIC  :: ProcPtr2
  END TYPE

  PROCEDURE(), POINTER :: ProcPtr1
  PROCEDURE(), POINTER :: ProcPtr2

  PRIVATE :: ProcPtr1
  PUBLIC  :: ProcPtr2

  TYPE(DT(4,20)) :: V
  TYPE(DT(4,20)) :: U

  PRIVATE  :: V
  PUBLIC   :: U

  CONTAINS

  SUBROUTINE Sub
  END SUBROUTINE

  SUBROUTINE ModSub()

  ProcPtr1 => Sub
  ProcPtr2 => Sub

  V%ProcPtr1 => Sub
  V%ProcPtr2 => Sub

  U%ProcPtr1 => Sub
  U%ProcPtr2 => Sub

  END SUBROUTINE

  END MODULE


  PROGRAM Misc20
  USE M
  IMPLICIT NONE

  ProcPtr1 => ModSub   ! no compalin here
  ProcPtr2 => ModSub

  V%ProcPtr1 => ModSub
  V%ProcPtr2 => ModSub !no compalin here due to err recovery
                       ! but when remove the complaint part,
                       !these err will be found
  U%ProcPtr1 => ModSub
  U%ProcPtr2 => ModSub


  END

