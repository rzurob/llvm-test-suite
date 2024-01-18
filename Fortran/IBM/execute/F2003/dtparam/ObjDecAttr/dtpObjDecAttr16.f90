!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr16
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 30, 2007
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
!*  INTRINSIC 
!*  
!*
!* 
!*  (337426)
!*   
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
    CHARACTER(L1+3) :: C1 = "DT1" 
    CONTAINS
    PROCEDURE(ModFun), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=K1
    INTEGER(K1), LEN     :: L2=K1
    CHARACTER(L2)        :: C2=CHAR(K2)
    INTEGER(K2)          :: I=K2
    REAL   (K2)          :: R=K2
    LOGICAL(K2)          :: L=.TRUE._1
    COMPLEX(K2)          :: Z=CMPLX(K1, K2, K2)
    TYPE(DT0(K2, L2))           :: T0 
    TYPE(DT2(K2, L2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun 
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  TYPE(DT2(8,*,8,*,8,*)), INTENT(IN) :: Arg
  TYPE(DT2(8,Arg%L0,8,Arg%L1,8,Arg%L2)) :: ModFun
     ModFun = DT2(8,8,8,8,8,8)(t0=dt0(8,Arg%L2)(), ptr2=null()) 
  END FUNCTION 

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT2(8,*,8,*,8,*)), INTENT(IN) :: Arg
  TYPE(DT2(8,Arg%L0,8,Arg%L1,8,Arg%L2)) :: ExtFun

    IF ( Arg%K0          .NE.   8     ) STOP 11
    IF ( Arg%K1          .NE.   8     ) STOP 12
    IF ( Arg%K2          .NE.   8     ) STOP 13
    IF ( Arg%L0          .NE.   8     ) STOP 14
    IF ( Arg%L1          .NE.   8     ) STOP 15
    IF ( Arg%L2          .NE.   8     ) STOP 16

    IF ( Arg%C1          .NE.   ''    ) STOP 17
    IF ( Arg%C2          .NE.   ''    ) STOP 17
    IF ( Arg%I           .NE.   0     ) STOP 18
    IF ( Arg%R           .NE.   0     ) STOP 19
    IF ( Arg%L                        ) STOP 21

    if ( (abs(real(Arg%Z,8)) >= 1.0d-31) .or.  &
         (abs(aimag(arg%z)) >= 1.0d-31)) stop 22

!    IF ( Arg%Z           .NE.   (0., 0.)     ) STOP 22
    IF ( Arg%T0%K0       .NE.   Arg%K2       ) STOP 23
    IF ( Arg%T0%L0       .NE.   Arg%L2       ) STOP 24
    IF ( .NOT. ASSOCIATED( Arg%Ptr2)         ) STOP 25

    ExtFun%C1 = "12345678" 
    ExtFun%C2 = "87654321" 
    ExtFun%I =  1 
    ExtFun%R =  1
    ExtFun%L = .TRUE. 
    ExtFun%Z =  (1.0, 1.0)
    ExtFun%T0 = DT0(8, Arg%L2)() 
    ExtFun%Ptr2 => NULL() 

  END FUNCTION

  PROGRAM dtpObjDecAttr16
  USE M

  PROCEDURE(ModFun) ExtFun 
  TYPE(DT2(8,:,8,:,8,:)), ALLOCATABLE :: A(:)
  TYPE(DT2(8,8,8,8,8,8)), TARGET :: T
  INTEGER :: I, N=99
  INTRINSIC MAX
  logical(4), external :: precision_x6

  INTERFACE MAX
    PROCEDURE ExtFun
  END INTERFACE

  T%C1 = ""
  T%C2 = ""
  T%I = 0
  T%R = 0.
  T%L = .FALSE.
  T%Z = (0.,0.)
  T%Ptr2 => T

  ALLOCATE(A(N), SOURCE=T) 

  IF( MAX(1,2,3,4,5,6) .NE. 6 )  STOP 55

  T = MAX(A(10))

    IF ( T%K0          .NE.   8     ) STOP 31
    IF ( T%K1          .NE.   8     ) STOP 32
    IF ( T%K2          .NE.   8     ) STOP 33
    IF ( T%L0          .NE.   8     ) STOP 34
    IF ( T%L1          .NE.   8     ) STOP 35
    IF ( T%L2          .NE.   8     ) STOP 36

    IF ( T%C1          .NE.   "12345678"    ) STOP 30
    IF ( T%C2          .NE.   "87654321"    ) STOP 37
    IF ( T%I           .NE.   1             ) STOP 38
    IF ( T%R           .NE.   1             ) STOP 39
    IF ( .not. T%L           ) STOP 41
    IF ( .not. precision_x6(T%Z, cmplx(1.0_8, 1.0_8,kind=8)) ) STOP 42
    IF ( T%T0%K0       .NE.   8             ) STOP 43
    IF ( T%T0%L0       .NE.   8             ) STOP 44
    IF ( ASSOCIATED( T%Ptr2 )               ) STOP 45

  END



