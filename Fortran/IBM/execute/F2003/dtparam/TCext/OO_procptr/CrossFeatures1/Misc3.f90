! GB DTP extension using:
! ftcx_dtp -qck /tstdev/OO_procptr/CrossFeatures1/Misc3.f
! opt variations: -qnock

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
!*  Pointer assignment
!*
!* (304184)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1,N1)    ! (1,3)
      INTEGER, KIND             :: K1
      INTEGER, LEN              :: N1
      CHARACTER(kind=K1,len=N1) :: C
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base(1,*)), INTENT(IN) :: Arg
        TYPE(Base(1,3)):: IntF(3)
      END FUNCTION
    END INTERFACE

  END MODULE


  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base(1,*)), INTENT(IN) :: Arg
  TYPE(Base(1,3)) :: ExtFun(3)
    ExtFun = Arg
  END FUNCTION


  PROGRAM Misc3
  USE M

  IMPLICIT TYPE(Base(1,3))(P)
  PROCEDURE(IntF) :: ExtFun

  PROCEDURE(IntF),       POINTER :: ProcPtr0
  PROCEDURE(TYPE(Base(1,3))), POINTER :: ProcPtr1
  PROCEDURE(),           POINTER :: ProcPtr2

  ProcPtr0 => ExtFun
  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtFun) ) ERROR STOP 10

  ProcPtr1 => ExtFun
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtFun) ) ERROR STOP 11

  ProcPtr2 => ExtFun
  IF ( .NOT. ASSOCIATED(ProcPtr2, ExtFun) ) ERROR STOP 12

  END


