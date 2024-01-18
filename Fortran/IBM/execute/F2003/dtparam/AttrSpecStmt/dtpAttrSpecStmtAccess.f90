!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtAccess
!*
!*  DATE                       : Jun. 08, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -- Accessibility
!*
!*  C549 (R519) Each use-name shall be the name of a named variable, procedure,
!*  derived type, named constant, or namelist group.
!*
!*  (337747)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  PRIVATE
  PUBLIC DT0, DT1, DT2
  PUBLIC N, T, Mutex, CT
  PUBLIC  MyThread1, MyThread2


  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND    :: K1=K0
    INTEGER(K0), LEN     :: L1=K0
    CHARACTER(L1+3)      :: C1 = "DT1"
    CONTAINS
    PROCEDURE(ModFun0), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=K1
    INTEGER(K1), LEN     :: L2=7
    CHARACTER(L2)        :: C2=CHAR(K2)
    INTEGER(K2)          :: I=K2
    REAL   (K2)          :: R=K2
    LOGICAL(K2)          :: L=.TRUE._1
    COMPLEX(K2)          :: Z=CMPLX(K1, K2, K2)
    TYPE(DT0(K2, L2))    :: T0(L2)
    TYPE(DT2(K0,L0,K1,L1,K2, L2)), POINTER  :: Ptr
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun0
  END TYPE


  INTEGER, PARAMETER            :: N = 1
  CLASS(DT0(1,:)), POINTER      :: T(:)
  INTEGER                       :: Mutex = 0


  TYPE(DT2(1,3,4,5,8,7)), PARAMETER ::             &
        CT =  DT2(1,3,4,5,8,7)   (                 &
                                  C1 = "XYZ",      &
                                  C2 = "ZYX",      &
                                   I = 1234,       &
                                   R = 4321.,      &
                                   L = .TRUE.,     &
                                   Z = (1.,-1.),   &
                                   Ptr  = NULL(),  &
                                   T0=DT0(8,7)() )


  EXTERNAL MyThread1, MyThread2

  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,1)), TARGET, INTENT(IN)  :: Arg(:)
  CLASS(DT0(1,1)), POINTER             :: ModFun0(:)
    ModFun0 => Arg
  END FUNCTION


  END MODULE


  PROGRAM dtpAttrSpecStmtAccess
  USE M
  uSE f_pthread
  IMPLICIT NONE

  INTEGER I, res
  VOLATILE  T

  TYPE(f_pthread_t)      ::  Thread1, thread2
  TYPE(f_pthread_attr_t) ::  Attr


  res = f_pthread_attr_init(Attr)

  if ((res == einval) .or. (res == enomem)) stop 199

  IF ( ASSOCIATED(T) ) STOP 10

  i = 1
  res = f_pthread_create(Thread1, Attr, FLAG_DEFAULT, MyThread1, I)

  if ((res == eagain) .or. (res == einval) .or. (res == enomem)) then
    PRINT *, "Something wrong in creating Thread1!"
    STOP 10
  END IF

  res = f_pthread_join (Thread1)

  if ((res == eagain) .or. (res == einval) .or. (res == enomem)) stop 100

  ! Verification
  IF ( .NOT. ASSOCIATED(T) ) STOP 11
  IF ( SIZE( T )  .NE. N   ) STOP 12
  if (Mutex /= 1) stop 13

  SELECT TYPE (T)
  TYPE IS (DT0(1,*))
  CLASS DEFAULT
    STOP 14
  END SELECT


  Mutex = 0

  i = 2
  res = f_pthread_create(Thread2, Attr, FLAG_DEFAULT, MyThread2, I)
  if ((res == eagain) .or. (res == einval) .or. (res == enomem)) stop 101

  res = f_pthread_join (Thread2)
  if ((res == eagain) .or. (res == einval) .or. (res == enomem)) stop 102

  IF ( .NOT. ASSOCIATED(T)   ) STOP 15
  IF ( LBOUND( T ,1)  .NE. N   ) STOP 16
  IF ( SIZE  ( T )  .NE. N   ) STOP 17

  SELECT TYPE (T)
  CLASS IS (DT2(1,*,4,*,8,*))
    DO I=N, 2*N-1
      IF ( T(I)%L0                .NE.   3        )  STOP 51
      IF ( T(I)%L1                .NE.   5        )  STOP 52
      IF ( T(I)%L2                .NE.   7        )  STOP 53
      IF ( T(I)%C1                .NE.   "XYZ"    )  STOP 54
      IF ( T(I)%C2                .NE.   "ZYX"    )  STOP 55
      IF ( T(I)%I                 .NE.   1234     )  STOP 56
      IF ( T(I)%R                 .NE.   4321.    )  STOP 57
      IF ( T(I)%L                 .NEQV. .TRUE.   )  STOP 58
      IF ( T(I)%Z                 .NE.   (1.,-1.) )  STOP 59
      IF ( T(I)%T0%K0             .NE.   8        )  STOP 60
      IF ( T(I)%T0%L0             .NE.   7        )  STOP 61
      IF ( SIZE(T(I)%T0)          .NE.   7        )  STOP 61
      IF ( ASSOCIATED(T(I)%Ptr )  .EQV.  .TRUE.   )  STOP 62
      IF ( T(I)%Ptr%K2            .NE.   8        )  STOP 63
      IF ( T(I)%Ptr%L2            .NE.   7        )  STOP 64
    END DO
  CLASS DEFAULT
    STOP 99
  END SELECT


  CALL f_pthread_exit()

  END

  SUBROUTINE MyThread1(I)
  USE M, only: t, dt0, mutex,n
  INTEGER I
    PRINT *, "The main has been waiting for ", i , "times!"
    CALL  sleep_(i)
    ! Allocating
    ALLOCATE(T(N), SOURCE=DT0(1,i)())
    Mutex = 1
  END SUBROUTINE


  SUBROUTINE MyThread2(I)
  USE M, only:t, ct, mutex,n
  INTEGER I
    PRINT *, "The main has been waiting for ", i , "times!"
    CALL  sleep_(i)
    !Change the type
    DEALLOCATE(T)
    ALLOCATE( T(N:2*N-1), SOURCE=CT)
    Mutex = 1
  END SUBROUTINE

