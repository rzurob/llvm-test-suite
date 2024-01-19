!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 06, 2007
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
!*  -- VOLATILE
!*
!*  An allocatable object with the VOLATILE attribute may additionally have its allocation
!*  status, dynamic type and type parameters, and array bounds changed by means not
!*  specified by the program.
!*
!*  Here we will change the properties by a FORTRAN thread.
!*
!*  (ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

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


  INTEGER, PARAMETER            :: N = 1113
  CLASS(DT0(1,:)), ALLOCATABLE  :: T(:)
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

  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,1)), TARGET, INTENT(IN)  :: Arg(:)
  CLASS(DT0(1,1)), POINTER             :: ModFun0(:)
    ModFun0 => Arg
  END FUNCTION


  END MODULE


  PROGRAM dtpObjDecAttr33
  USE M
  uSE f_pthread
  IMPLICIT NONE

  INTEGER I, res, j
  VOLATILE  mutex !Note from JX: this volatile attribute is needed to have the
                  !busy spin loop works -- code updated 2008-06-19

  TYPE(f_pthread_t)      ::  Thread1, thread2
  TYPE(f_pthread_attr_t) ::  Attr

  EXTERNAL MyThread1, mythread2

  res = f_pthread_attr_init(Attr)

  if ((res == einval) .or. (res == enomem)) error stop 100

  IF ( ALLOCATED(T) ) ERROR STOP 10

  i = 0
  res = f_pthread_create(Thread1, Attr, FLAG_DEFAULT, MyThread1, I)

  if ((res == einval) .or. (res == enomem) .or. (res == eagain)) error stop 101

  DO WHILE ( Mutex .EQ. 0 ) !<-- NOTE from JX: this is a busy spin loop
    j = j+1
  END DO

  ! Verification
  IF ( .NOT. ALLOCATED(T) ) ERROR STOP 11
  IF ( SIZE( T )  .NE. N  ) ERROR STOP 12

  SELECT TYPE (T)
  TYPE IS (DT0(1,*))
  CLASS DEFAULT
    STOP 14
  END SELECT


  Mutex = 0
  I = 1

  res = f_pthread_create(Thread2, Attr, FLAG_DEFAULT, MyThread2, I)

  if ((res == einval) .or. (res == enomem) .or. (res == eagain)) error stop 102

  DO WHILE ( Mutex .EQ. 0 )
    j = j+1
  END DO

  IF ( .NOT. ALLOCATED(T)   ) ERROR STOP 15
  IF ( LBOUND( T, 1 )  .NE. N  ) ERROR STOP 16
  IF ( SIZE  ( T )  .NE. N  ) ERROR STOP 17

  SELECT TYPE (T)
  CLASS IS (DT2(1,*,4,*,8,*))
    DO I=N, 2*N-1
      IF ( T(I)%L0                .NE.   3        )  ERROR STOP 51
      IF ( T(I)%L1                .NE.   5        )  ERROR STOP 52
      IF ( T(I)%L2                .NE.   7        )  ERROR STOP 53
      IF ( T(I)%C1                .NE.   "XYZ"    )  ERROR STOP 54
      IF ( T(I)%C2                .NE.   "ZYX"    )  ERROR STOP 55
      IF ( T(I)%I                 .NE.   1234     )  ERROR STOP 56
      IF ( T(I)%R                 .NE.   4321.    )  ERROR STOP 57
      IF ( T(I)%L                 .NEQV. .TRUE.   )  ERROR STOP 58
      IF ( T(I)%Z                 .NE.   (1.,-1.) )  ERROR STOP 59
      IF ( T(I)%T0%K0             .NE.   8        )  ERROR STOP 60
      IF ( T(I)%T0%L0             .NE.   7        )  ERROR STOP 61
      IF ( SIZE(T(I)%T0)          .NE.   7        )  ERROR STOP 61
      IF ( ASSOCIATED(T(I)%Ptr )  .EQV.  .TRUE.   )  ERROR STOP 62
      IF ( T(I)%Ptr%K2            .NE.   8        )  ERROR STOP 63
      IF ( T(I)%Ptr%L2            .NE.   7        )  ERROR STOP 64
    END DO
  CLASS DEFAULT
    STOP 99
  END SELECT


  CALL f_pthread_exit()

  END

  SUBROUTINE MyThread1(I)
  USE M
  INTEGER I
    PRINT *, "The main has been waiting for ", i , "times!"
    CALL  sleep_(1)
    ! Allocating
    ALLOCATE(T(N), SOURCE=DT0(1,i)())
    Mutex = 1
  END SUBROUTINE


  SUBROUTINE MyThread2(I)
  USE M
  INTEGER I
    PRINT *, "The main has been waiting for ", i , "times!"
    CALL  sleep_(1)
    !Change the type
    DEALLOCATE(T)
    ALLOCATE( T(N:2*N-1), SOURCE=CT)
    Mutex = 1
  END SUBROUTINE

