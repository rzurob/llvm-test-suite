!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 08, 2006
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
!*  pointer component
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT
    INTEGER(1),   POINTER :: PtrI1(:, :)
    INTEGER(2),   POINTER :: PtrI2(:)
    INTEGER(1),   PRIVATE :: I1Tar(10,10)=1_1
    INTEGER(2),   PRIVATE :: I2Tar(100)=2_1
  CONTAINS
    PROCEDURE, NOPASS :: Fun => ModFun
    PROCEDURE, NOPASS :: Fun1 => ModFun1
  END TYPE

  TYPE (DT), SAVE, TARGET   :: T(10,10)

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT),  TARGET, INTENT(IN)  :: Arg(:,:)
  INTEGER(1), POINTER :: ModFun(:, :)
    ModFun(SIZE(Arg,1):,SIZE(Arg,2):) => Arg(1,1)%I1Tar
  END FUNCTION

  FUNCTION ModFun1(Arg)
  CLASS(DT),  TARGET, INTENT(IN) :: Arg(:)
  INTEGER(2), POINTER :: ModFun1(:)
    ModFun1(LBOUND(Arg,1):UBOUND(Arg,1))  => Arg(1)%I2Tar
  END FUNCTION

  END MODULE

  PROGRAM dataPtrProcPtrComp
  USE M
  IMPLICIT NONE

  T(1,1)%PtrI1(0:, 0: ) => T%Fun(T)
  IF (.NOT. ASSOCIATED(T(1,1)%PtrI1))                       ERROR STOP 11
  IF (ANY( LBOUND(T(1,1)%PtrI1)         .NE. (/0, 0 /)))    ERROR STOP 12
  IF (ANY( UBOUND(T(1,1)%PtrI1)         .NE. (/9, 9 /)))    ERROR STOP 13
  IF (ANY( T(1,1)%PtrI1                 .NE. 1_1))          ERROR STOP 14

  T(10,10)%PtrI2(9: )=> NULL(T(10,10)%PtrI2 )
  T(10,10)%PtrI2(0:9 ) => T(:, 1)%Fun1(T(:, 1))
  IF (.NOT. ASSOCIATED(T(10,10)%PtrI2))                     ERROR STOP 15
  IF (ANY( LBOUND(T(10,10)%PtrI2)         .NE. (/0 /)))     ERROR STOP 16
  IF (ANY( UBOUND(T(10,10)%PtrI2)         .NE. (/9 /)))     ERROR STOP 17
  IF (ANY( T(10,10)%PtrI2                 .NE. 2_1))        ERROR STOP 18

  END

