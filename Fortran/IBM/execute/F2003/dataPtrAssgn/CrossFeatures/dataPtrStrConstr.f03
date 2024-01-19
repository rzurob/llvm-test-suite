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
!*  Construction of derived-type values
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


  TYPE :: DT
    CLASS(*), POINTER :: Ptr(:, :)
  CONTAINS
    PROCEDURE, NOPASS :: Fun => ModFun
  END TYPE

  TYPE :: DT1
    CLASS(*), POINTER :: Ptr(:)
  CONTAINS
    PROCEDURE, NOPASS :: Fun => ModFun1
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT),  TARGET :: Arg(:, :)
  TYPE(DT), POINTER :: ModFun(:, :)
    ModFun(LBOUND(Arg,1):,LBOUND(Arg,2):) => Arg
    !ModFun => Arg
  END FUNCTION

  FUNCTION ModFun1(Arg)
  CLASS(DT1),  TARGET, INTENT(IN) :: Arg(:)
  TYPE(DT1), POINTER :: ModFun1(:)
    ModFun1(LBOUND(Arg,1):UBOUND(Arg,1)-1) => Arg
    !ModFun1 => Arg
  END FUNCTION

  END MODULE

  PROGRAM dataPtrStrConstr
  USE M
  IMPLICIT NONE

  TYPE (DT),  TARGET   :: T(10,10)
  TYPE (DT1), TARGET   :: T1(10)

  T = DT(T%Fun(T))
  SELECT TYPE( As => T(1,1)%Ptr)
  TYPE IS (DT)
    IF (.NOT. ASSOCIATED(T(1,1)%Ptr, T))                    ERROR STOP 11
    IF (ANY( LBOUND(T(1,1)%Ptr)         .NE. (/1, 1 /)))    ERROR STOP 12
    IF (ANY( UBOUND(T(1,1)%Ptr)         .NE. (/10,10 /)))    ERROR STOP 13
  CLASS DEFAULT
     STOP 14
  END SELECT


  T1 = DT1(T1%Fun(T1))
  SELECT TYPE( As => T1(1)%Ptr)
  TYPE IS (DT1)
    IF (.NOT. ASSOCIATED(T1(1)%Ptr, T1(1:9)))           ERROR STOP 21
    IF (ANY( LBOUND(T1(1)%Ptr)         .NE. (/1 /)))    ERROR STOP 22
    IF (ANY( UBOUND(T1(1)%Ptr)         .NE. (/9 /)))    ERROR STOP 23
  CLASS DEFAULT
     STOP 14
  END SELECT

  END


