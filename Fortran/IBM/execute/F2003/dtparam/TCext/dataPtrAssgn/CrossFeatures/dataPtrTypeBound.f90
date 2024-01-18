! GB DTP extension using:
! ftcx_dtp -qk /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrTypeBound.f
! opt variations: -qck -qnok

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 09, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature Number 289075
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  type bound procedure
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K1,N1)    ! (4,3)
    INTEGER, KIND :: K1
    INTEGER, LEN  :: N1
    CHARACTER(N1) :: CArr(0:9, 0:9)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT    ! (4,3)
  CONTAINS
    PROCEDURE, NOPASS :: Fun => ModFun
    PROCEDURE, NOPASS :: Fun1 => ModFun1
  END TYPE

  TYPE (DT(4,3)), SAVE, TARGET   :: T

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT(4,*)),  TARGET :: Arg
  CHARACTER(3), POINTER :: ModFun(:, :)
    ModFun(LBOUND(Arg%CArr,1):,LBOUND(Arg%CArr,2):) => Arg%CArr
  END FUNCTION

  FUNCTION ModFun1(Arg)
  CLASS(DT(4,*)),  TARGET, INTENT(IN) :: Arg
  CHARACTER(3), POINTER :: ModFun1(:)
    ModFun1(LBOUND(Arg%CArr,1):UBOUND(Arg%CArr,1)) => Arg%CArr(:, 1)
  END FUNCTION

  END MODULE

  PROGRAM dataPtrTypeBound
  USE M
  IMPLICIT NONE

  CHARACTER(3), POINTER :: PtrCArr(:, :)

  T%CArr = "123"
  PtrCArr => T%Fun(T)

  IF (.NOT. ASSOCIATED(PtrCArr))                       STOP 11
  IF (ANY( LBOUND(PtrCArr)         .NE. (/0, 0 /)))    STOP 12
  IF (ANY( UBOUND(PtrCArr)         .NE. (/9, 9 /)))    STOP 13
  IF (ANY( PtrCArr                 .NE. "123"))        STOP 14

  PtrCArr(1:, 1:) => T%Fun(T)
  IF (.NOT. ASSOCIATED(PtrCArr))                       STOP 21
  IF (ANY( LBOUND(PtrCArr)         .NE. (/ 1, 1 /)))   STOP 22
  IF (ANY( UBOUND(PtrCArr)         .NE. (/10,10 /)))   STOP 23
  IF (ANY( PtrCArr                 .NE. "123"))        STOP 24

  PtrCArr(0:9, 0:0 ) => T%Fun1(T)
  IF (.NOT. ASSOCIATED(PtrCArr))                       STOP 15
  IF (ANY( LBOUND(PtrCArr)         .NE. (/0, 0 /)))    STOP 16
  IF (ANY( UBOUND(PtrCArr)         .NE. (/9, 0 /)))    STOP 17
  IF (ANY( PtrCArr                 .NE. "123"))        STOP 18

  END


