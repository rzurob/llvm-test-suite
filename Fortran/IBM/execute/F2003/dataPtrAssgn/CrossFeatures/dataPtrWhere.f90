!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrWhere.f
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
!*  where
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrWhere
  IMPLICIT NONE

  CLASS(*), POINTER :: Tar1(:), Tar2(:, :)
  CLASS(*), POINTER :: Ptr(:, :)
  LOGICAL,  POINTER :: L1(:),  L2(:, :)
  LOGICAL,  POINTER :: L1Tar(:),  L2Tar(:, :)
  INTEGER    :: I, J, K, N

  N = 100; K = 0

  ALLOCATE(Tar2(N,N), SOURCE="123")
  ALLOCATE(Tar1(N*N), SOURCE=(1.0,-1.0))
  ALLOCATE(L1Tar(N*N), SOURCE=.TRUE.)
  ALLOCATE(L2Tar(N,N), SOURCE=.TRUE.)

  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => Tar2
    L2 (I:, J:) => L2Tar
    SELECT TYPE (Ptr)
    TYPE IS (CHARACTER(*))
      WHERE (L2)
        Ptr  = CHAR(I)//CHAR(I)//CHAR(J)
      END WHERE
    CLASS DEFAULT
      STOP 10
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr, Tar2))             STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) STOP 13
    SELECT TYPE(Tar2)
    TYPE IS (CHARACTER(*))
      IF (ANY( Tar2      .NE.  CHAR(I)//CHAR(I)//CHAR(J)  ))  STOP 14
    END SELECT

    Ptr(I:J, I:J) => Tar1
    L2(I:J, I:J)  => L1Tar
    SELECT TYPE (Ptr)
    TYPE IS (COMPLEX)
      WHERE (L2)
        Ptr = (I, J)
      END WHERE
    CLASS DEFAULT
      STOP 20
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr))                 STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      STOP 23
    SELECT TYPE(Tar1)
    TYPE IS (COMPLEX)
      IF (ANY( Tar1(1:(J-I+1)*(J-I+1)) .NE. (I, J) ))  STOP 24
    CLASS DEFAULT
      STOP 25
    END SELECT

  END DO
  END DO

  DEALLOCATE(Tar2)
  DEALLOCATE(Tar1)

  END


