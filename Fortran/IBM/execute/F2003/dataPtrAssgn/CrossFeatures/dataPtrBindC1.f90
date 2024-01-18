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
!*  derived type with bind(C)
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrBindC1
  USE ISO_C_BINDING
  IMPLICIT NONE

  TYPE, BIND(C) :: DT
    INTEGER(C_INT) :: I
    REAL(C_FLOAT)  :: R
  END TYPE

  TYPE(DT), ALLOCATABLE,  TARGET  :: Tar2(:, :)
  TYPE(DT), ALLOCATABLE,  TARGET  :: Tar1(:)
  TYPE(DT), POINTER               :: Ptr(:, :)
  INTEGER    :: I, J, K, N

  N = 100; K = 0
  ALLOCATE(Tar1(N*N))
  ALLOCATE(Tar2(N, N))

  Tar2%R = RESHAPE((/((i*J,i=1,N), j=1, N)/), (/N,N/))
  Tar2%I = RESHAPE((/((i*J,i=1,N), j=1, N)/), (/N,N/))
  Tar1%R = (/(i,i=1,N*N)/)
  Tar1%I = (/(i,i=1,N*N)/)

  DO I =1, N
  DO J =I, N

    Ptr(I:, J:) => Tar2

    IF ( ANY( MAXLOC(Ptr%R)  .NE. (/N, N/)) )  ERROR STOP 11
    IF ( ANY( MAXLOC(Ptr%I)  .NE. (/N, N/)) )  ERROR STOP 12
    IF ( ANY( MINLOC(Ptr%R)  .NE. (/1, 1/)) )  ERROR STOP 13
    IF ( ANY( MINLOC(Ptr%I)  .NE. (/1, 1/)) )  ERROR STOP 14
    CALL Check2()


    Ptr(I:J, I:J) => Tar1

    IF ( ANY( MAXLOC(Ptr%R)  .NE. (/J-I+1, J-I+1/)) )  ERROR STOP 21
    IF ( ANY( MAXLOC(Ptr%I)  .NE. (/J-I+1, J-I+1/)) )  ERROR STOP 22
    IF ( ANY( MINLOC(Ptr%R)  .NE. (/1, 1/)) )  ERROR STOP 23
    IF ( ANY( MINLOC(Ptr%I)  .NE. (/1, 1/)) )  ERROR STOP 24
    CALL Check1()

  END DO
  END DO

  CONTAINS

  SUBROUTINE Check1()
    IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))            ERROR STOP 30
    IF (.NOT. ASSOCIATED(Ptr))                      ERROR STOP 31
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))           ERROR STOP 32
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))           ERROR STOP 33
  END SUBROUTINE

  SUBROUTINE Check2()
    IF (SIZE(Ptr)  .NE. N*N )                    ERROR STOP 40
    IF (.NOT. ASSOCIATED(Ptr, Tar2))             ERROR STOP 41
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        ERROR STOP 42
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) ERROR STOP 43
  END SUBROUTINE

  END



