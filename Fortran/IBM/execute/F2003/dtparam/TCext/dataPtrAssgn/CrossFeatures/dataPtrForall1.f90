! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/CrossFeatures/dataPtrForall1.f
! opt variations: -qnok -ql

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrForall1.f  
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



  PROGRAM dataPtrForall1 
  IMPLICIT NONE

  CHARACTER(3), TARGET :: Tar2(10, 10), Tar1(100)
  TYPE   :: DT0(K1)    ! (4)
      INTEGER, KIND :: K1
    CLASS(*), POINTER :: Ptr(:, :)
  END TYPE

  TYPE, EXTENDS(DT0) :: DT    ! (4)
  END TYPE

  TYPE(DT(4)) :: T(10,10)

  LOGICAL           :: L(10, 10) 
  INTEGER    :: I, J, K, N
 
  N = 10; K = 0

  Tar1 = "???"
  Tar2 = "???"

  L = .TRUE.

  FORALL (I=1:N, J=1:N)
    T(I,J)%Ptr => Tar2
    T(I,J)%Ptr(I:, J:) => T(I,J)%Ptr
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

  FORALL (I=1:3, J=I:3)
    T(I,J)%Ptr(1:N, 1:N) => Tar1 
    T(I,J)%Ptr(I:J, I:J) => T(I,J)%Ptr(1,1:N)
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

  CONTAINS

  PURE FUNCTION Check1(Ptr, Arr, I, J)
  CHARACTER(3), TARGET, INTENT(IN) :: Arr(:, :)
  CLASS(*), POINTER, INTENT(IN) :: Ptr(:, :)
  INTEGER,           INTENT(IN) :: I, J
  LOGICAL                       :: Check1

    Check1 = .TRUE.

    SELECT TYPE (Ptr)
    TYPE IS (CHARACTER(*))
!     Ptr = REPEAT(CHAR(I), 3)  ! violate C1272
    CLASS DEFAULT
      Check1 = .FALSE. 
    END SELECT
 
    IF (.NOT. ASSOCIATED(Ptr, Arr))                 Check1 = .FALSE. 
    IF (ANY( LBOUND(Ptr) .NE. (/I, J /)))           Check1 = .FALSE. 
    IF (ANY( UBOUND(Ptr) .NE. (/I+N-1, J+N-1/)))    Check1 = .FALSE. 
!   IF (ANY( Arr         .NE.  REPEAT(CHAR(I), 3))) Check1 = .FALSE. 

  END FUNCTION

  PURE FUNCTION Check2(Ptr, Arr, I, J)
  CHARACTER(3), TARGET, INTENT(IN) :: Arr(:)
  CLASS(*), POINTER, INTENT(IN) :: Ptr(:, :)
  INTEGER,           INTENT(IN) :: I, J
  LOGICAL                       :: Check2

    Check2 = .TRUE.

    SELECT TYPE (Ptr)
    TYPE IS (CHARACTER(*))
!     Ptr = REPEAT(CHAR(J), 3)   ! violate C1272
    CLASS DEFAULT
      Check2 = .FALSE. 
    END SELECT
 
    IF (.NOT. ASSOCIATED(Ptr))                  Check2 = .FALSE. 
    IF (ANY( LBOUND(Ptr) .NE. (/I,  I/)))       Check2 = .FALSE. 
    IF (ANY( UBOUND(Ptr) .NE. (/J,  J/)))       Check2 = .FALSE. 
!   IF (ANY( Arr(1:(J-I+1)*(J-I+1)) .NE. REPEAT(CHAR(J), 3) ))  Check2 = .FALSE. 
 
  END FUNCTION

  END


