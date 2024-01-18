! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Nullify1.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Nullify1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Nullify1.f
!*
!*  DATE                       : May. 10, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  The nullify stmt
!*  in the same NULLIFY statement
!*  (ICE-304576)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id
      PROCEDURE(INTEGER), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Nullify1
  USE M
  IMPLICIT NONE

  TYPE(DT(4)), POINTER       :: V1, V2
  PROCEDURE(Fun), POINTER :: ProcPtr

  TYPE(DT(4)), POINTER       :: W(:)

  ALLOCATE(V1)
  V1 = DT(4)(-1, Fun)
  NULLIFY(V1%ProcPtr)

  !ALLOCATE(V2, SOURCE=DT(-1, Fun)) ! not 10.1
  ALLOCATE(V2)
  V2 = DT(4)(-1, Fun)
! NULLIFY(V1%ProcPtr, V1%ProcPtr)
  NULLIFY(V1%ProcPtr)
  DEALLOCATE(V1)

  !ALLOCATE(W(3), SOURCE=DT(-1, Fun))
  ALLOCATE(W(3))
  W = DT(4)(-1, Fun)
  NULLIFY(W(1)%ProcPtr, W(2)%ProcPtr, W(3)%ProcPtr)
  DEALLOCATE(W)

  END



