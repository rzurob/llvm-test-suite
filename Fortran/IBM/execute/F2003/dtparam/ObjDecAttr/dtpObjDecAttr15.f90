!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr15
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
!*  no intent
!*  
!*
!* 
!*  (init expr issue)
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER, KIND    :: K1=K0
    INTEGER, LEN     :: L1=K0
    CHARACTER(L1+3) :: C1 = "DT1" 
    CONTAINS
    PROCEDURE(ModFun), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER, KIND    :: K2=K1
    INTEGER, LEN     :: L2=K1
    CHARACTER(L2)        :: C2=''!CHAR(K2)
    INTEGER(K2)          :: I=K2
    REAL   (K2)          :: R=K2
    LOGICAL(K2)          :: L=.TRUE._1
    COMPLEX(K2)          :: Z=(K1, K2)
    TYPE(DT0(K2, L2))           :: T0 
    TYPE(DT2(K2, L2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun 
  END TYPE

  CHARACTER(10) :: C

  CONTAINS

  FUNCTION ModFun(Arg)
  TYPE(DT2(8,*,8,*,8,*)) :: Arg(:) 
  TYPE(DT2(8,Arg%L0,8,Arg%L1,8,Arg%L2)) :: ModFun(SIZE(Arg))
     ModFun = dt2(8,Arg%L0,8,Arg%L1,8,Arg%L2)(t0=dt0(8,Arg%L2)(), ptr2=null())
  END FUNCTION 

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT2(8,*,8,*,8,*)) :: Arg(:) 
  TYPE(DT2(8,Arg%L0,8,Arg%L1,8,Arg%L2)) :: ExtFun(SIZE(Arg))

    IF ( Arg%K0          .NE.   8     ) STOP 11
    IF ( Arg%K1          .NE.   8     ) STOP 12
    IF ( Arg%K2          .NE.   8     ) STOP 13
    IF ( Arg%L0          .NE.   8     ) STOP 14
    IF ( Arg%L1          .NE.   8     ) STOP 15
    IF ( Arg%L2          .NE.   8     ) STOP 16

    IF ( ANY(Arg%C1          .NE.   ''    )) STOP 17
    IF ( ANY(Arg%C2          .NE.   ''    )) STOP 17
    IF ( ANY(Arg%I           .NE.   0     )) STOP 18
    IF ( ANY(Arg%R           .NE.   0     )) STOP 19
    IF ( ANY(Arg%L                        )) STOP 21
    IF ( ANY(Arg%Z           .NE.   (0., 0.) )   ) STOP 22
    IF ( Arg%T0%K0            .NE.   Arg%K2       ) STOP 23
    IF ( Arg%T0%L0            .NE.   Arg%L2       ) STOP 24
    IF ( .NOT. ASSOCIATED( Arg(1)%Ptr2)          ) STOP 25

    ExtFun%C1 = "12345678" 
    ExtFun%C2 = "87654321" 
    ExtFun%I =  1 
    ExtFun%R =  1
    ExtFun%L = .TRUE. 
    ExtFun%Z =  (1, 1)
    ExtFun%T0 = DT0(8, Arg%L2)() 
    do i = 1, size(arg)
        nullify(ExtFun(i)%Ptr2)
    end do

    C = "ExtFun"

  END FUNCTION

  PROGRAM dtpObjDecAttr15
  USE M

  PROCEDURE(ModFun) ExtFun 
  TYPE(DT2(8,:,8,:,8,:)), ALLOCATABLE :: A(:)
  TYPE(DT2(8,8,8,8,8,8)), TARGET :: T(71), Tar(71) 
  INTEGER I

  Tar%C1 = ''
  Tar%C2 = ''
  Tar%I = 0
  Tar%R = 0.
  Tar%L = .FALSE.
  Tar%Z = (0.,0.)

  do i = 1, 71
    Tar(i)%Ptr2 => Tar(i)
  end do

  ALLOCATE(A(71), SOURCE=Tar) 

  T =  ExtFun(Tar)
  Tar = ExtFun(A)

  DO I=1, SIZE(A)

    IF ( Tar(I)%K0          .NE.   8     ) STOP 31
    IF ( Tar(I)%K1          .NE.   8     ) STOP 32
    IF ( Tar(I)%K2          .NE.   8     ) STOP 33
    IF ( Tar(I)%L0          .NE.   8     ) STOP 34
    IF ( Tar(I)%L1          .NE.   8     ) STOP 35
    IF ( Tar(I)%L2          .NE.   8     ) STOP 36

    IF ( Tar(I)%C1          .NE.   "12345678"    ) STOP 30
    IF ( Tar(I)%C2          .NE.   "87654321"    ) STOP 37
    IF ( Tar(I)%I           .NE.   1             ) STOP 38
    IF ( Tar(I)%R           .NE.   1             ) STOP 39
    IF ( .not. Tar(I)%L              ) STOP 41
    IF ( Tar(I)%Z           .NE.   (1, 1)        ) STOP 42
    IF ( Tar(I)%T0%K0       .NE.   8               ) STOP 43
    IF ( Tar(I)%T0%L0       .NE.   8               ) STOP 44
    IF ( ASSOCIATED( Tar(I)%Ptr2 )                      ) STOP 45

    IF ( C  .NE.   "ExtFun"      ) STOP 46

  END DO

  END

