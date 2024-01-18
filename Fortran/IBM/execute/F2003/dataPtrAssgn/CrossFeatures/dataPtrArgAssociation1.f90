!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrArgAssociation1.f
!*
!*  DATE                       : Feb. 20, 2006
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
!*  Argument
!*
!*  (323280)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrArgAssociation1
  IMPLICIT NONE

  INTEGER,  TARGET  :: Tar2(100, 100)
  INTEGER,  TARGET  :: Tar1(10000)
  INTEGER, POINTER  :: Ptr(:, :)
  INTEGER    :: I, J, K, N


  N = 100; K = 0
  Tar1 = 1
  Tar2 = 2

  DO I =1, N
  DO J =I, N

    Ptr(F2(Ptr, Tar2, I, J):, F1(Ptr, Tar1, I, J):) => Tar2
    Ptr = I*J
    IF (SIZE(Ptr)  .NE. N*N )                    STOP 10
    IF (.NOT. ASSOCIATED(Ptr, Tar2))             STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/-I, -J /)))      STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) STOP 13
    IF (ANY( Tar2     .NE.  I*J ))               STOP 14

    Ptr(F1(Ptr, Tar1, I, J):F2(Ptr, Tar2, I, J), I:J) => Tar1
    Ptr = -I*J

    IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))            STOP 20
    IF (.NOT. ASSOCIATED(Ptr))                      STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/-J,  I/)))          STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/-I,  J/)))          STOP 23
    IF (ANY( Tar1(1:(J-I+1)*(J-I+1)) .NE.  -I*J ))  STOP 24

  END DO
  END DO

  CONTAINS

  FUNCTION F1(Ptr, Arr, I, J)
  INTEGER  :: I, J, F1
  INTEGER, POINTER :: Ptr(:, :)
  INTEGER, TARGET  :: Arr(:)
    Ptr (I:J, I:J) => Arr
    F1 = -J
  END FUNCTION

  FUNCTION F2(Ptr, Arr, I, J)
  INTEGER  :: I, J, F2
  INTEGER, POINTER :: Ptr(:, :)
  INTEGER, TARGET  :: Arr(:, :)
    Ptr (I:, J:) => Arr
    F2 = -I
  END FUNCTION

  END

