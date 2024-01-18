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
!*  Entry
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrEntry
  IMPLICIT NONE

  INTEGER, ALLOCATABLE,  TARGET  :: Tar2(:, :)
  INTEGER, ALLOCATABLE,  TARGET  :: Tar1(:)
  CLASS(*), POINTER  :: Ptr(:, :)
  INTEGER    :: I, J, K, N

  INTERFACE
    FUNCTION  ExtFun11(Arr, I, J, N)
      INTEGER  :: I, J, N
      CLASS(*), POINTER :: ExtFun11(:, :)
      INTEGER, TARGET  :: Arr(N*N)
    END FUNCTION

    FUNCTION ExtFun21(Arr, I, J, N)
      INTEGER  :: I, J, N
      CLASS(*), POINTER :: ExtFun21(:, :)
      INTEGER, TARGET  :: Arr(N, N)
    END FUNCTION
  END INTERFACE


  N = 100; K = 0
  ALLOCATE(Tar1(N*N),  SOURCE=-1)
  ALLOCATE(Tar2(N, N), SOURCE=-2)

  DO I =1, N
  DO J =I, N
    Ptr => ExtFun21(Tar2(1,1), I, J, N)
    IF (.NOT. ASSOCIATED(Ptr, Tar2))               ERROR STOP 10

    SELECT TYPE( Ptr )
    TYPE IS (INTEGER)
      Ptr = I*J
      IF (SIZE(Ptr)  .NE. N*N )                    ERROR STOP 11
      IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        ERROR STOP 12
      IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) ERROR STOP 13
      IF (ANY( Tar2     .NE.  I*J ))               ERROR STOP 14
    CLASS DEFAULT
      STOP 15
    END SELECT

    Ptr => ExtFun11(Tar1(1), I, J, N)
    IF (.NOT. ASSOCIATED(Ptr))                        ERROR STOP 20

    SELECT TYPE( Ptr )
    TYPE IS (INTEGER)
      Ptr = -I*J
      IF (SIZE(Ptr)  .NE. (J-I+1)*(J-I+1))            ERROR STOP 21
      IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))           ERROR STOP 22
      IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))           ERROR STOP 23
      IF (ANY( Tar1(1:(J-I+1)*(J-I+1)) .NE.  -I*J ))  ERROR STOP 24
    CLASS DEFAULT
      STOP 15
    END SELECT

  END DO
  END DO

  END

  FUNCTION  ExtFun1(Arr, I, J, N)
  INTEGER  :: I, J, N
  CLASS(*), POINTER :: ExtFun1(:, :), ExtFun11(:, :)
  INTEGER, TARGET  :: Arr(N*N)
    ExtFun1(I:J, I:J) => Arr
    RETURN
  ENTRY ExtFun11(Arr, I, J, N)
    ExtFun11(I:J, I:J) => Arr
  END FUNCTION

  FUNCTION ExtFun2(Arr, I, J, N)
  INTEGER  :: I, J, N
  CLASS(*), POINTER :: ExtFun2(:, :), ExtFun21(:, :)
  INTEGER, TARGET  :: Arr(N, N)
    ExtFun2(I:, J:) => Arr
    RETURN
  ENTRY ExtFun21(Arr, I, J, N)
    ExtFun21(I:, J:) => Arr
  END FUNCTION


