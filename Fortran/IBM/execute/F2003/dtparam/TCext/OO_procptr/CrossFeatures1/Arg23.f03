! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/Arg23.f
! opt variations: -qnock

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
!*  Argument association - Implicit interface
!*  Dummy procedure pointer: the associated actual argument shall be
!*  a procedure pointer, a reference to a function that returns a procedurei
!*  pointer, or a reference to the NULL intrinsic function.
!*
!*  (related to 304020)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  IMPLICIT TYPE(Base(1,3))(P)

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
    END TYPE

    INTERFACE
      SUBROUTINE IntF(Arg1, Arg2)
      IMPORT
        TYPE(Base(1,*)), INTENT(IN)  :: Arg2
        TYPE(Base(1,*)), INTENT(OUT) :: Arg1
      END SUBROUTINE
    END INTERFACE

  CONTAINS

    SUBROUTINE ModSub1(ProcPtr)
    PROCEDURE(IntF), POINTER :: ProcPtr
      IF ( ASSOCIATED(ProcPtr)) ERROR STOP 11
    END SUBROUTINE

    SUBROUTINE ModSub2(ProcPtr)
    PROCEDURE(), POINTER :: ProcPtr  !Implies a subroutine
      IF ( ASSOCIATED(ProcPtr)) ERROR STOP 12
    END SUBROUTINE

    SUBROUTINE ModSub3(ProcPtr)
    IMPLICIT TYPE(Base(1,3))(P)
    PROCEDURE(), POINTER :: ProcPtr
      IF ( ASSOCIATED(ProcPtr)) ERROR STOP 13
    END SUBROUTINE

  END MODULE


  PROGRAM Arg23
  USE M
  IMPLICIT NONE
  PROCEDURE(IntF) :: ExtSub
  PROCEDURE(IntF), POINTER :: ProcPtr

  CALL ModSub1(NULL())
  CALL ModSub2(NULL())
  CALL ModSub3(NULL())

  ProcPtr => ExtSub
  CALL ModSub1(NULL(ProcPtr))
! CALL ModSub2(NULL(ProcPtr))  ! Since ProcPtr is a function
! CALL ModSub3(NULL(ProcPtr))

  END

  SUBROUTINE ExtSub(Arg1, Arg2)
  USE M
  TYPE(Base(1,*)), INTENT(IN)  :: Arg2
  TYPE(Base(1,*)), INTENT(OUT) :: Arg1
    Arg1 = Arg2
  END SUBROUTINE
