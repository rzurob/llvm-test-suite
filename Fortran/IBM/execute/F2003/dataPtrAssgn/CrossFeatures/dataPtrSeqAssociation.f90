!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSeqAssociation.f
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
!*  sequence association
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrSeqAssociation
  IMPLICIT NONE

  INTEGER,  TARGET  :: Tar2(100, 100)
  INTEGER,  TARGET  :: Tar1(10000)
  CLASS(*), POINTER  :: Ptr(:, :)
  INTEGER    :: I, J, K, N


  N = 100; K = 0
  Tar1 = -1
  Tar2 = -2

  DO I =1, N
  DO J =I, N

    CALL Sub2(Ptr, Tar2(1,1), I, J, N)
    IF (.NOT. ASSOCIATED(Ptr, Tar2))               STOP 11

    SELECT TYPE( Ptr )
    TYPE IS (INTEGER)
      Ptr = I*J
      IF (SIZE(Ptr)  .NE. N*N )                    STOP 10
      IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        STOP 12
      IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) STOP 13
      IF (ANY( Tar2     .NE.  I*J ))               STOP 14
    CLASS DEFAULT
      STOP 15
    END SELECT

    CALL Sub1(Ptr, Tar1(1), I, J, N)
    IF (.NOT. ASSOCIATED(Ptr))                        STOP 20

    SELECT TYPE( Ptr )
    TYPE IS (INTEGER)
      Ptr = -I*J
      IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))            STOP 21
      IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))           STOP 22
      IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))           STOP 23
      IF (ANY( Tar1(1:(J-I+1)*(J-I+1)) .NE.  -I*J ))  STOP 24
    CLASS DEFAULT
      STOP 15
    END SELECT

  END DO
  END DO

  CONTAINS

  SUBROUTINE  Sub1(Ptr, Arr, I, J, N)
  INTEGER  :: I, J, N
  CLASS(*), POINTER :: Ptr(:, :)
  INTEGER, TARGET  :: Arr(N*N)
    Ptr (I:J, I:J) => Arr
  END SUBROUTINE

  SUBROUTINE Sub2(Ptr, Arr, I, J, N)
  INTEGER  :: I, J, N
  CLASS(*), POINTER :: Ptr(:, :)
  INTEGER, TARGET  :: Arr(N, N)
    Ptr (I:, J:) => Arr
  END SUBROUTINE

  END

