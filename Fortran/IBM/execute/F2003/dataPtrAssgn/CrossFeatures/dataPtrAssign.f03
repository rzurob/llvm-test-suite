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
!*  Assignment
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrAssign
  IMPLICIT NONE

  CLASS(*), POINTER :: Tar1(:), Tar2(:, :)
  CLASS(*), POINTER :: Ptr(:, :)

  INTEGER    :: I, J, K, N

  N = 100; K = 0

  ALLOCATE(Tar2(N,N), SOURCE="123")
  ALLOCATE(Tar1(N*N), SOURCE=(1.0,-1.0))

  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => Tar2
    SELECT TYPE (Ptr)
    TYPE IS (CHARACTER(*))
      Ptr = "321"
    CLASS DEFAULT
      STOP 10
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr, Tar2))             ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) ERROR STOP 13
    SELECT TYPE(Tar2)
    TYPE IS (CHARACTER(*))
      IF (ANY( Tar2      .NE.  "321" ))          ERROR STOP 14
    END SELECT

    Ptr(I:J, I:J) => Tar1
    SELECT TYPE (Ptr)
    TYPE IS (COMPLEX)
      Ptr = (-1.0, 1.0)
    CLASS DEFAULT
      STOP 20
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr))                 ERROR STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      ERROR STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      ERROR STOP 23
    SELECT TYPE(Tar1)
    TYPE IS (COMPLEX)
      IF (ANY( Tar1(1:(J-I+1)*(J-I+1)) .NE.  (-1.0, 1.0) ))  ERROR STOP 24
    CLASS DEFAULT
      STOP 25
    END SELECT

  END DO
  END DO

  DEALLOCATE(Tar2)
  DEALLOCATE(Tar1)

  END

