!*********************************************************************
!*  ===================================================================
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
!*  Array reduction functions
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrArrReductionFun
  IMPLICIT NONE

  INTEGER, ALLOCATABLE,  TARGET  :: Tar2(:, :)
  INTEGER, ALLOCATABLE,  TARGET  :: Tar1(:)
  INTEGER, POINTER               :: Ptr(:, :)
  INTEGER    :: I, J, K, N


  N = 100; K = 0
  ALLOCATE(Tar1(N*N),  SOURCE=-1)
  ALLOCATE(Tar2(N, N), SOURCE=-2)

  DO I =1, N
  DO J =I+1, N

    Ptr(I:, J:) => Tar2
    Ptr = I*J
    IF ( .NOT. ALL( Ptr .EQ.  I*J ))               ERROR STOP 31
    IF (       ANY( Ptr .NE.  I*J ))               ERROR STOP 32
    IF ( COUNT( Ptr .EQ.  I*J ) .NE. N*N)          ERROR STOP 33
    IF ( SUM( Ptr ) .NE.  I*J*N*N )                ERROR STOP 34
    CALL Check2()

    Ptr(I:J, I:J) => Tar1
    Ptr = -I*J; Ptr(J,J) = I*J; Ptr(I,I) = -J*J
    IF ( MAXVAL( Ptr ) .NE.  I*J )                ERROR STOP 41
    IF ( MINVAL( Ptr ) .NE. -J*J )                ERROR STOP 42
    CALL Check1()

  END DO
  END DO

  CONTAINS

  SUBROUTINE Check1()
    IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))            ERROR STOP 20
    IF (.NOT. ASSOCIATED(Ptr))                      ERROR STOP 21
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))           ERROR STOP 22
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))           ERROR STOP 23
  END SUBROUTINE

  SUBROUTINE Check2()
    IF (SIZE(Ptr)  .NE. N*N )                    ERROR STOP 10
    IF (.NOT. ASSOCIATED(Ptr, Tar2))             ERROR STOP 11
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        ERROR STOP 12
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) ERROR STOP 13
  END SUBROUTINE

  END



