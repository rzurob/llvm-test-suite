!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 21, 2006
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
!*  CopyPrivate
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrCopyPrivate
  IMPLICIT NONE


  INTEGER,  TARGET  :: Tar2(10, 10)
  INTEGER,  POINTER :: Tar1(:)
  INTEGER,  POINTER :: Ptr(:, :)
  INTEGER    :: I, J, K, N

  N = 10; K = 0

  ALLOCATE(Tar1(N*N))
  Tar2 = RESHAPE((/((i*J,i=1,N), j=1, N)/), (/N, N/))
  Tar1 = (/(i,i=1,N*N)/)

  !$OMP PARALLEL PRIVATE(I, J, K, Ptr) SHARED(Tar1)
  DO I =1, N
  DO J =I, N
    !$OMP SINGLE
    Ptr(I:, J:) => Tar2
    !$OMP END SINGLE COPYPRIVATE(Ptr)

    DO K=1, I
      Ptr(K:, K:) => Ptr
      IF ( ANY( MAXLOC(Ptr)  .NE. (/N, N/)) )      STOP 11
      IF ( ANY( MINLOC(Ptr)  .NE. (/1, 1/)) )      STOP 12
      IF (SIZE(Ptr)  .NE. N*N )                    STOP 40
      IF (.NOT. ASSOCIATED(Ptr, Tar2))             STOP 41
      IF (ANY( LBOUND(Ptr) .NE. (/K, K /)))        STOP 42
      IF (ANY( UBOUND(Ptr) .NE. (/K+N-1, K+N-1/))) STOP 43
    END DO


    !$OMP SINGLE
      Tar1(I+J:N*N+I+J-1) => Tar1
    !$OMP END SINGLE   COPYPRIVATE(Ptr)

    DO K=1, I
      Ptr(K:I, K:J) => Tar1
      IF ( ANY( MAXLOC(Ptr)  .NE. (/I-K+1, J-K+1/)) ) STOP 21
      IF ( ANY( MINLOC(Ptr)  .NE. (/1, 1/)) )         STOP 22
      IF (SIZE(Ptr)  .NE. (I-K+1)*(J-K+1))            STOP 30
      IF (.NOT. ASSOCIATED(Ptr))                      STOP 31
      IF (ANY( LBOUND(Ptr) .NE. (/K,  K/)))           STOP 32
      IF (ANY( UBOUND(Ptr) .NE. (/I,  J/)))           STOP 33
    END DO

  END DO
  END DO
  !$OMP END PARALLEL


  END



