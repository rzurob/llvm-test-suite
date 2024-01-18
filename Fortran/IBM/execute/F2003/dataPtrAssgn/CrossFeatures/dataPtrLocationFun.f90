!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrLocationFun.f
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
!*  Array location functions
!*
!*  (320355)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrLocationFun
  IMPLICIT NONE

  CHARACTER(:), ALLOCATABLE,  TARGET  :: Tar2(:, :)
  CHARACTER(:), ALLOCATABLE,  TARGET  :: Tar1(:)
  CHARACTER(:), POINTER               :: Ptr(:, :)
  INTEGER    :: I, J, K, N

  N = 10; K = 0
  ALLOCATE(CHARACTER ::Tar1(N*N))
  ALLOCATE(CHARACTER ::Tar2(N, N))

  Tar2 = RESHAPE((/((ACHAR(i*J),i=1,N), j=1, N)/), (/N,N/))
  Tar1 = (/(ACHAR(I),i=1,N*N)/)

  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => Tar2
    IF ( ANY( MAXLOC(Ptr)  .NE. (/N, N/)) )  STOP 11
    IF ( ANY( MINLOC(Ptr)  .NE. (/1, 1/)) )  STOP 12
    CALL Check2()


    Ptr(I:J, I:J) => Tar1

    IF ( ANY( MAXLOC(Ptr)  .NE. (/J-I+1, J-I+1/)) )  STOP 21
    IF ( ANY( MINLOC(Ptr)  .NE. (/1, 1/)) )          STOP 22
    CALL Check1()

  END DO
  END DO

  CONTAINS

  SUBROUTINE Check1()
    IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))            STOP 30
    IF (.NOT. ASSOCIATED(Ptr))                      STOP 31
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))           STOP 32
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))           STOP 33
  END SUBROUTINE

  SUBROUTINE Check2()
    IF (SIZE(Ptr)  .NE. N*N )                    STOP 40
    IF (.NOT. ASSOCIATED(Ptr, Tar2))             STOP 41
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        STOP 42
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) STOP 43
  END SUBROUTINE

  END



