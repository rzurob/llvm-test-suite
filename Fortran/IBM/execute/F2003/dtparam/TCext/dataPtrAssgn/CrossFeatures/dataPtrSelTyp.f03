! GB DTP extension using:
! ftcx_dtp -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrSelTyp.f
! opt variations: -qck -qdefaultpv -qnodeferredlp -qreuse=none

!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 16, 2006
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
!*  Select type
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(N1,K1)    ! (3,4)
    INTEGER, KIND               :: K1
    INTEGER, LEN                :: N1
    CHARACTER(N1), PRIVATE      :: C="???"
    INTEGER(K1)                 :: ID
    CLASS(DT(:,K1)), POINTER    :: Ptr(:, :)
    CONTAINS
    PROCEDURE, NOPASS :: ModFun1
    PROCEDURE, NOPASS :: ModFun2
  END TYPE

  TYPE, EXTENDS(DT) :: DT1    ! (3,4)
    CHARACTER(N1), PRIVATE :: CC="???"
  END TYPE

  CONTAINS

  FUNCTION ModFun1(Arg)
  CLASS(DT(*,4)), TARGET, INTENT(IN) :: Arg(:)
  CLASS(DT(:,4)),  POINTER    :: ModFun1(:)
    ModFun1 => Arg
  END FUNCTION

  FUNCTION ModFun2(Arg)
  CLASS(DT(*,4)), TARGET, INTENT(IN) :: Arg(:, :)
  CLASS(DT(:,4)),  POINTER    :: ModFun2(:, :)
    ModFun2 => Arg
  END FUNCTION

  END MODULE

  PROGRAM dataPtrSelTyp
  USE M
  IMPLICIT NONE

  TYPE(DT(3,4)), TARGET  ::  Tar2(100, 100)
  TYPE(DT1(3,4)), TARGET  :: Tar1(10000)
  CLASS(DT(:,4)), POINTER :: Ptr(:, :)
  PROCEDURE(ModFun1), POINTER  :: Ptr1
  PROCEDURE(ModFun2), POINTER  :: Ptr2


  INTEGER    :: I, J, K, N

  N = 100; K = 0
  Ptr1 => ModFun1
  Ptr2 => ModFun2

  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => Ptr2(Tar2)
    SELECT TYPE(Ptr)
    TYPE IS (DT(*,4))
      Ptr = DT(3,4)(ID=I*J, Ptr=Tar2)
    END SELECT

    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) ERROR STOP 13
    IF (ANY( Tar2%ID     .NE.  I*J ))            ERROR STOP 14
    IF (.NOT. ASSOCIATED(Ptr, Tar2))               ERROR STOP 11

    Ptr(I:J, I:J) => Ptr1(Tar1)
    SELECT TYPE (Ptr)
    TYPE IS (DT1(*,4))
      Ptr = DT1(3,4)(ID=-I*J, Ptr=Tar2)
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr))                   ERROR STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      ERROR STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      ERROR STOP 23
    IF (ANY( Tar1(1:(J-I+1)*(J-I+1))%ID .NE.  -I*J ))  ERROR STOP 24

  END DO
  END DO


  END


