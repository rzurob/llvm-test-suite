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
!*  (323314)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrAssign1
  IMPLICIT NONE

  CHARACTER(:), TARGET, ALLOCATABLE :: Tar2(:,:), Tar1(:)
  CLASS(*), POINTER :: Ptr(:, :)

  INTEGER    :: I, J, K, N

  N = 64; K = 0

  ALLOCATE(Tar1(N*N), SOURCE="!!!")
  ALLOCATE(Tar2(N, N), SOURCE="???")


  DO I =1, N
  DO J =I, N
    Tar2 = "???"

    Ptr => Tar2
    Ptr(I:, J:) => Ptr

    SELECT TYPE (Ptr)
    TYPE IS (CHARACTER(*))
      Ptr(I,J) = REPEAT(CHAR(I), 3)
      Ptr(I+N-1,J+N-1) = REPEAT(CHAR(I), 3)
    CLASS DEFAULT
      STOP 10
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr, Tar2))             ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) ERROR STOP 13
    IF (     Tar2(1,1)        .NE.  REPEAT(CHAR(I), 3))    ERROR STOP 14
    IF (ANY( Tar2(1,2:N)      .NE.  REPEAT("?",     3)))   ERROR STOP 15
    IF (ANY( Tar2(2:N-1,:)    .NE.  REPEAT("?",     3)))   ERROR STOP 16
    IF (ANY( Tar2(N,1:N-1)    .NE.  REPEAT("?",     3)))   ERROR STOP 17
    IF (     Tar2(N,N)        .NE.  REPEAT(CHAR(I), 3))    ERROR STOP 18

    Tar1 = "!!!"
    Ptr(1:, 1:) => NULL()
    Ptr(I:J, I:J) => Tar1
    SELECT TYPE (Ptr)
    TYPE IS (CHARACTER(*))
      Ptr(I,I) = REPEAT(CHAR(I), 3)
      Ptr(J,J) = REPEAT(CHAR(I), 3)
    CLASS DEFAULT
      STOP 20
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr))                 ERROR STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      ERROR STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      ERROR STOP 23
    IF (     Tar1(1)                      .NE.  REPEAT(CHAR(I), 3))    ERROR STOP 24
    IF (ANY( Tar1(2:(J-I+1)*(J-I+1)-1)    .NE.  REPEAT("!",   3)))     ERROR STOP 25
    IF (     Tar1((J-I+1)*(J-I+1))        .NE.  REPEAT(CHAR(I), 3))    ERROR STOP 26
    IF (ANY( Tar1((J-I+1)*(J-I+1)+1:)     .NE.  REPEAT("!",     3)))   ERROR STOP 27

  END DO
  END DO


  END

