! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Misc19.f
! opt variations: -qnol

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Misc19.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             :  Misc19.f
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
!*  Stucture cnstructor overwritting
!*
!*  (Jim-267618)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  Module M

  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    SEQUENCE
    INTEGER(K1)   :: ID
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  TYPE(DT(20,4)) :: ModFun, ArG
    ModFun = Arg
  END FUNCTION

  SUBROUTINE ModSub()
  END SUBROUTINE

  END MODULE

  FUNCTION ExtFun (Arg)
  TYPE :: DT(N1,K1)    ! (20,4)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    SEQUENCE
    INTEGER(K1)   :: ID
    PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr
  END TYPE

  TYPE(DT(*,4)) :: Arg(:)
  TYPE(DT(20,4)) :: ExtFun(SIZE(Arg))

    ExtFun = Arg

  END FUNCTION

  PROGRAM Misc19
  USE M
  IMPLICIT NONE

  INTERFACE  DT

    FUNCTION ExtFun (Arg)
      IMPORT
      TYPE(DT(*,4)) :: Arg(:)
      TYPE(DT(20,4)) :: ExtFun(SIZE(Arg))
    END FUNCTION

    MODULE PROCEDURE ModFun

  END INTERFACE

  TYPE(DT(20,4)) :: U, V(3000)
  INTEGER  :: I

  U = DT(DT(20,4)(-1, ModSub) )
  IF ( U%Id .NE. -1 )                          STOP 22
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ModSub) )   STOP 23

  V = DT((/(DT(20,4)(-1, ModSub), i=1,3000)/) )
  DO I  = 1, 3000
    IF ( V(I)%Id .NE. -1 )                          STOP 32
    IF ( .NOT. ASSOCIATED(V(I)%ProcPtr, ModSub) )   STOP 33
  END DO


  END


