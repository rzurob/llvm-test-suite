! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_procptr/CrossFeatures1/Associated4.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 9, 2005
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
!*  ASSOCIATED(POINTER [, TARGET])
!*  TARGET is a procedure pointer
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE :: DT(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: Id=0
    END TYPE

    CONTAINS

    FUNCTION FProcPtr(Arg)
    PROCEDURE(TYPE(DT(20,4))), POINTER :: FPRocPtr
    PROCEDURE(TYPE(DT(20,4)))          :: Arg
      FProcPtr => Arg
    END FUNCTION

    FUNCTION FDT()
    TYPE(DT(20,4)) :: FDT
      FDT = DT(20,4)(-1)
    END FUNCTION

  END MODULE


  PROGRAM Associated4
  USE M
  IMPLICIT NONE
  PROCEDURE(TYPE(DT(20,4))), POINTER :: ProcPtr=>NULL()
  PROCEDURE(TYPE(DT(20,4))), POINTER :: ProcPtr1=>NULL()


  IF ( ASSOCIATED( ProcPtr, NULL(ProcPtr)))   STOP 11

  ProcPtr => FDT
  IF ( .NOT. ASSOCIATED( ProcPtr, ProcPtr))   STOP 12

  IF ( .NOT. ASSOCIATED( ProcPtr, FProcPtr(ProcPtr)))  STOP 13
  IF ( .NOT. ASSOCIATED( ProcPtr, FProcPtr(FDT)))      STOP 14

  ProcPtr => ProcPtr
  PRINT*, ProcPtr()

  IF ( ASSOCIATED( ProcPtr, ProcPtr1))  STOP 15
  IF ( ASSOCIATED( ProcPtr1, ProcPtr))  STOP 16

 END

