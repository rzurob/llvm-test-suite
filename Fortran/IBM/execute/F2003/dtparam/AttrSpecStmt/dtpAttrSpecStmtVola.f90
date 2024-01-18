!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtVola
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 13, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
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
!* 
!*  -- VOLATILE Statement 
!*  
!*
!* 
!*  ()
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE, ABSTRACT :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    CONTAINS
    PROCEDURE(MyThread), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND    :: K1=K0
    INTEGER(K0), LEN     :: L1=K0
    CHARACTER(L1+3)      :: C1 = "DT1"
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=K1
    INTEGER(K1), LEN     :: L2=7
    CHARACTER(L2)        :: C2=CHAR(K2)
    INTEGER(K2)          :: I=K2
    REAL   (K2)          :: R=K2
    LOGICAL(K2)          :: L=.TRUE._1
    COMPLEX(K2)          :: Z=CMPLX(K1, K2, K2)
    TYPE(DT2(K0,L0,K1,L1,K2,L2)), POINTER  :: Ptr
    CONTAINS
    PROCEDURE, NOPASS :: Proc => MyThread
  END TYPE


  INTEGER, PARAMETER           :: N = 1113
  TYPE(DT2(1,3,4,5,8,7)), SAVE :: T(N)
  INTEGER                      :: Mutex = 0


  TYPE(DT2(1,3,4,5,8,7)), PARAMETER ::             &
        CT =  DT2(1,3,4,5,8,7)   (                 &
                                  C1 = "XYZ",      &
                                  C2 = "ZYX",      &
                                   I = 1234,       &
                                   R = 4321.,      &
                                   L = .TRUE.,     &
                                   Z = (1.,-1.),   &
                                   Ptr  = NULL() )

  INTERFACE
    SUBROUTINE MyThread(I)
      INTEGER I 
    END SUBROUTINE
  END INTERFACE


  END MODULE


  PROGRAM dtpAttrSpecStmtVola
  USE M
  USE f_pthread
  IMPLICIT NONE

  INTEGER I

  TYPE(f_pthread_t)      ::  Thread
  TYPE(f_pthread_attr_t) ::  Attr 

  CALL IntSub()

  CONTAINS

  SUBROUTINE IntSub()

  IF (f_pthread_attr_init(Attr) .NE. 0 ) THEN
    PRINT *, "Init Attr failed!"
    STOP 11
  END IF

  IF ( f_pthread_create(Thread, Attr, FLAG_DEFAULT, MyThread, I) .ne. 0 ) THEN 
    PRINT *, "Something wrong in creating a thread!"
    STOP 12
  END IF

  I = 0
  DO WHILE ( Mutex .EQ. 0 )
    I = I+1 
  END DO 

  ! Verification
  DO I=1, N 
    IF ( T(I)%L0                .NE.   3        )  STOP 51
    IF ( T(I)%L1                .NE.   5        )  STOP 52
    IF ( T(I)%L2                .NE.   7        )  STOP 53
    IF ( T(I)%C1                .NE.   "XYZ"    )  STOP 84
    IF ( T(I)%C2                .NE.   "ZYX"    )  STOP 85
    IF ( T(I)%I                 .NE.   1234     )  STOP 86
    IF ( T(I)%R                 .NE.   4321.    )  STOP 87
    IF ( T(I)%L                 .NEQV. .TRUE.   )  STOP 88
    IF ( T(I)%Z                 .NE.   (1.,-1.) )  STOP 89
    IF ( ASSOCIATED(T(I)%Ptr )  .EQV.  .TRUE.   )  STOP 92
    IF ( T(I)%Ptr%K2            .NE.   8        )  STOP 93
    IF ( T(I)%Ptr%L2            .NE.   7        )  STOP 94
  END DO

  CALL f_pthread_exit()

  END SUBROUTINE

  END

  SUBROUTINE MyThread(I)
  USE M, ONLY: T, CT, Mutex
  INTEGER I 
    PRINT *, "The main has been waiting for ", i , "times!"
    CALL  sleep_(1)
    T = CT
    Mutex = 1
  END SUBROUTINE

