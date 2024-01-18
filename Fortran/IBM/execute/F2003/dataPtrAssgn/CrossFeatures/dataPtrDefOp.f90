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
!*  Defined operator
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  INTEGER, PRIVATE    :: I, J, K, N

  INTERFACE OPERATOR( + )
    MODULE PROCEDURE F1, F2
  END INTERFACE

  CONTAINS

  FUNCTION F1(Arg1, Arg2)
  CHARACTER(*), INTENT(IN)  :: Arg1(:, :), Arg2(:,:)
  CHARACTER(LEN(Arg1)+LEN(Arg2)), POINTER :: F1(:, :)
    ALLOCATE(F1(SIZE(Arg1, 1), SIZE(Arg1, 2)))
    DO I = 1, SIZE(Arg1,1)
    DO J = 1, SIZE(Arg1,1)
      F1(I, J) = Arg1(I,J)//Arg2(I,J)
    END DO
    END DO
  END FUNCTION

  FUNCTION F2(Arg1, Arg2)
  CHARACTER(*), INTENT(IN)  :: Arg1(:), Arg2(:)
  CHARACTER(LEN(Arg1)+LEN(Arg2)), POINTER :: F2(:)
    ALLOCATE(F2(SIZE(Arg1)))
    DO I = 1, SIZE(Arg1)
      F2(I) = Arg1(I)//Arg2(I)
    END DO
  END FUNCTION


  END MODULE


  PROGRAM dataPtrDefOp
  USE M
  IMPLICIT NONE

  CHARACTER(7), TARGET  :: T11(100, 100),T21(10000), C1
  CHARACTER(:), POINTER :: Ptr(:,:)
  CHARACTER(8), TARGET  :: T12(100, 100),T22(10000), C2
  INTEGER    :: I, J, K, N

  N = 100; K = 0
  C1 = "1234567"
  C2 = "76543210"
  T11 = C1
  T12 = C2
  T21 = C1
  T22 = C2

  ! N is too big
  DO I =1, N-90
  DO J =I, N-90
    Ptr(I:, J:) => T11 + T12
    IF (.NOT. ASSOCIATED(Ptr))                   STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) STOP 13
    IF (LEN(Ptr)         .NE. 15)                STOP 14
    IF (ANY( Ptr         .NE.  C1//C2 ))         STOP 15

    Ptr(I:J, I:J) => T21 + T22
    IF (.NOT. ASSOCIATED(Ptr))                 STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      STOP 23
    IF (LEN(Ptr)         .NE. 15)              STOP 24
    IF (ANY( Ptr         .NE.  C1//C2 ))       STOP 25

  END DO
  END DO

  END


