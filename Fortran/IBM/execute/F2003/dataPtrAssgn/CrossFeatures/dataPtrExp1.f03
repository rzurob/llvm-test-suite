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
!*  Expression - type parameter
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  FUNCTION F1(Arg)
  CHARACTER(*), TARGET  :: Arg(:, :)
  CHARACTER(LEN(Arg)), POINTER :: F1(:, :)
    F1(SIZE(Arg, 1):, SIZE(Arg, 2):) => Arg
! nullify(f1)
  END FUNCTION

  FUNCTION F2(Arg)
  CHARACTER(*), TARGET  :: Arg(:)
  CHARACTER(LEN(Arg)), POINTER :: F2(:)
    F2(SIZE(Arg, 1):) => Arg
! nullify(f2)
  END FUNCTION

  END MODULE


  PROGRAM dataPtrExp1
  USE M
  IMPLICIT NONE

  CHARACTER(15), TARGET  :: T(100, 100), C
  CHARACTER(:), POINTER  :: Ptr(:,:)
  CHARACTER(15), TARGET  :: T1(10000)
  INTEGER    :: I, J, K, N

  N = 100; K = 0
  C = "012345678912345"
  T = C
  T1 = C


  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => F1(T)
    IF (.NOT. ASSOCIATED(Ptr))                   ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) ERROR STOP 13
    IF (LEN(Ptr) .NE. 15)                        ERROR STOP 14
    IF (ANY( Ptr      .NE.  C ))                 ERROR STOP 15

    Ptr(I:J, I:J) => F2(T1)
    IF (.NOT. ASSOCIATED(Ptr))                 ERROR STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      ERROR STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      ERROR STOP 23
    IF (LEN(Ptr) .NE. 15)                      ERROR STOP 24
    IF (ANY( Ptr      .NE.  C ))               ERROR STOP 25

  END DO
  END DO

  END


