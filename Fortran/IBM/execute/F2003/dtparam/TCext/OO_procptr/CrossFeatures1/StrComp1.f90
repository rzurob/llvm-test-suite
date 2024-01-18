! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/OO_procptr/CrossFeatures1/StrComp1.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

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
!*  Structure component - sequence type
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1)    ! (4)
        INTEGER, KIND :: K1
      SEQUENCE
      PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
    END TYPE

    TYPE  :: DT(K2)    ! (4)
      INTEGER, KIND  :: K2
      SEQUENCE
      PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
      TYPE(Base(K2)) :: BComp
    END TYPE


    CONTAINS

    FUNCTION RToR(Arg)
    REAL(8) :: Arg
    REAL(8) :: RToR
      RToR = Arg
    END FUNCTION

  END MODULE


  SUBROUTINE ExtSub(Arg1, Arg2 )

  TYPE :: Base(K3)    ! (4)
      INTEGER, KIND :: K3
    SEQUENCE
    PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
  END TYPE

  TYPE  :: DT(K4)    ! (4)
    INTEGER, KIND  :: K4
    SEQUENCE
    PROCEDURE(REAL(8)), NOPASS, POINTER :: ProcPtr=>NULL()
    TYPE(Base(K4)) :: BComp
  END TYPE

  REAL(8)  :: Arg2
  TYPE(DT(4)) :: Arg1

    IF ( .NOT. ASSOCIATED(Arg1%ProcPtr)) STOP 11
    IF ( .NOT. ASSOCIATED(Arg1%BComp%ProcPtr)) STOP 12

    IF ( Arg1%ProcPtr(Arg2) .NE. Arg2 ) STOP 13
    IF ( Arg1%BComp%ProcPtr(Arg2) .NE. Arg2 ) STOP 14

  END SUBROUTINE


  PROGRAM StrComp1
  USE M
  IMPLICIT NONE

  PROCEDURE(RToR), POINTER   :: ProcPtr

  INTERFACE
    SUBROUTINE ExtSub(Arg1, Arg2 )
      IMPORT DT
      REAL(8)  :: Arg2
      TYPE(DT(4)) :: Arg1
    END SUBROUTINE
  END INTERFACE

  TYPE(DT(4)) :: V

  V%ProcPtr => RToR
  V%BComp%ProcPtr => RToR

  CALL ExtSub(V, 8.0_8)
  CALL ExtSub(V, -8.0_8)

  END

