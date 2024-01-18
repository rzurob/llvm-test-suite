!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrForall.f  
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 17, 2006
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature Number 289075 
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  
!*  Forall 
!*
!*  
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM dataPtrForall 
  IMPLICIT NONE
  
  CLASS(*), POINTER :: Tar1(:), Tar2(:, :)

  TYPE :: DT
    CLASS(*), POINTER :: Ptr(:, :)
  END TYPE

  TYPE(DT) :: T(10, 10)

  INTEGER           :: Bound1(10,10), Bound2(10, 10)
  LOGICAL           :: L(10, 10)
  INTEGER    :: I, J, K, N
 
  N = 10; K = 0

  ALLOCATE(Tar2(N,N), SOURCE="123")
  ALLOCATE(Tar1(N*N), SOURCE=(1.0,-1.0))

  L = .TRUE.
 
  FORALL (I=1:N, J=1:N)
    T(I,J)%Ptr(I:, J:) => Tar2
    L (I, J) = Check1(T(I,J)%Ptr, Tar2, I, J)
  END FORALL 

  DO I = 1, N
  DO J = 1, N
    IF (.NOT. L(I, J) ) THEN
      PRINT *, I, J
      STOP 11
    END IF
  END DO
  END DO
 
  L = .TRUE.
 
  FORALL (I=1:N, J=I:N)
    T(I,J)%Ptr(I:J, I:J) => Tar1 
    L (I, J) = Check2(T(I,J)%Ptr, Tar1, I, J)
  END FORALL

  DO I = 1, N
  DO J = I, N
    IF (.NOT. L(I, J) ) THEN
      PRINT *, I, J
      STOP 12
    END IF
  END DO
  END DO
 
  DEALLOCATE(Tar2)
  DEALLOCATE(Tar1)

  CONTAINS

  PURE FUNCTION Check1(Ptr, Arr, I, J)
  CLASS(*), POINTER, INTENT(IN) :: Arr(:, :) 
  CLASS(*), POINTER, INTENT(IN) :: Ptr(:, :)
  INTEGER,           INTENT(IN) :: I, J
  LOGICAL                       :: Check1

    Check1 = .TRUE.

    SELECT TYPE (Ptr)
    TYPE IS (CHARACTER(*))
 !    Ptr = "321"  ! violate C1272
    CLASS DEFAULT 
      Check1 = .FALSE.
    END SELECT

    IF (.NOT. ASSOCIATED(Ptr, Tar2))             Check1 = .FALSE. 
 !  IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))        Check1 = .FALSE. 
 !  IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/))) Check1 = .FALSE. 
 !  SELECT TYPE(Arr)
 !  TYPE IS (CHARACTER(*))
 !    IF (ANY( Arr      .NE.  "321" ))           Check1 = .FALSE. 
 !  CLASS DEFAULT
 !    Check1 = .FALSE.
 !  END SELECT

  END FUNCTION

  PURE FUNCTION Check2(Ptr, Arr, I, J)
  CLASS(*), POINTER, INTENT(IN) :: Arr(:) 
  CLASS(*), POINTER, INTENT(IN) :: Ptr(:, :)
  INTEGER,           INTENT(IN) :: I, J
  LOGICAL                       :: Check2

    Check2 = .TRUE.

    SELECT TYPE (Ptr)
    TYPE IS (COMPLEX)
 !    Ptr = (-1.0, 1.0) ! violate C1272 
    CLASS DEFAULT
      Check2 = .FALSE.
    END SELECT
 
    IF (.NOT. ASSOCIATED(Ptr))                 Check2 = .FALSE. 
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))      Check2 = .FALSE. 
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))      Check2 = .FALSE. 
    SELECT TYPE(Arr)
    TYPE IS (COMPLEX)
 !    IF (ANY( Arr(1:(J-I+1)*(J-I+1)) .NE.  (-1.0, 1.0) )) Check2 = .FALSE. 
    CLASS DEFAULT
      Check2 = .FALSE. 
    END SELECT

  END FUNCTION

  END


