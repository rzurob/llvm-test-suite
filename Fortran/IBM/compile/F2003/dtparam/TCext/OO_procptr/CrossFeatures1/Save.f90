! GB DTP extension using:
! ftcx_dtp -qk /tstdev/OO_procptr/CrossFeatures1/Save.f
! opt variations: -qck -qnok

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 07, 2005
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
!*  Save
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM Save
  IMPLICIT TYPE(Base(4,3))(P)

  TYPE :: Base(K1,N1)    ! (4,3)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: C
  END TYPE

  CONTAINS

    SUBROUTINE IntSub1(Proc)
    SAVE        :: Proc
    PROCEDURE() :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub2(Proc)
    SAVE        :: Proc
    EXTERNAL    :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub3(Proc)
    SAVE                  :: Proc
    PROCEDURE(TYPE(Base(4,3))) :: Proc
    END SUBROUTINE

    SUBROUTINE IntSub4()
    EXTERNAL    :: Proc
    SAVE        :: Proc
    END SUBROUTINE

  END


