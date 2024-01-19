!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 31, 2007
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
!*  OPTIONAL
!*
!*  ()
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
    PROCEDURE(ModSub), NOPASS, DEFERRED :: Proc
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
    TYPE(DT2(k0,l0,k1,l1,K2, L2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Arg11, Arg12, Arg21, Arg22, Arg31, Arg32)
  TYPE(DT0(8,:)),         ALLOCATABLE, OPTIONAL :: Arg11
  CLASS(DT1(8,*,8,:)),    ALLOCATABLE, OPTIONAL :: Arg21
  TYPE(DT2(8,:,8,*,8,:)), ALLOCATABLE, OPTIONAL :: Arg31
  TYPE(DT0(8,*)),optional         :: Arg12
  TYPE(DT2(8,*,8,*,8,*)),optional :: Arg22
  TYPE(DT2(8,*,8,*,8,*)),optional :: Arg32

  IF( PRESENT(Arg11) ) then
  if ( ALLOCATED(Arg11) ) THEN
    IF ( Arg11%K0   .NE.   Arg12%K0     ) ERROR STOP 11
    IF ( Arg11%L0   .NE.   Arg12%L0     ) ERROR STOP 12
  END IF
  END IF

  IF( PRESENT(Arg21) ) THEN
  IF( ALLOCATED(Arg21) ) THEN

    SELECT TYPE ( Arg21 )
    TYPE IS (DT2(8,*,8,*,8,*))

      IF ( Arg21%K0   .NE.   Arg22%K0     ) ERROR STOP 21
      IF ( Arg21%L0   .NE.   Arg22%L0     ) ERROR STOP 22
      IF ( Arg21%K1   .NE.   Arg22%K1     ) ERROR STOP 23
      IF ( Arg21%L1   .NE.   Arg22%L1     ) ERROR STOP 24
      IF ( Arg21%K2   .NE.   Arg22%K2     ) ERROR STOP 25
      IF ( Arg21%L2   .NE.   Arg22%L2     ) ERROR STOP 26

      IF ( Arg21%C1   .NE.   Arg22%C1     ) ERROR STOP 27
      IF ( Arg21%C2   .NE.   Arg22%C2     ) ERROR STOP 28
      IF ( Arg21%I    .NE.   Arg22%I      ) ERROR STOP 29
      IF ( Arg21%R    .NE.   Arg22%R      ) ERROR STOP 30
      IF ( Arg21%L    .nEQV.  Arg22%L      ) ERROR STOP 31
      IF ( Arg21%Z    .NE.   Arg22%Z      ) ERROR STOP 32
      IF ( Arg21%T0%K0.NE.   Arg22%T0%K0  ) ERROR STOP 33
      IF ( Arg21%T0%L0.NE.   Arg22%T0%L0  ) ERROR STOP 34

      IF ( ASSOCIATED( Arg21%Ptr2) .nEQV. ASSOCIATED( Arg22%Ptr2)  ) ERROR STOP 35
      IF ( Arg21%Ptr2%K2           .NE.   Arg22%Ptr2%K2           ) ERROR STOP 36
      IF ( Arg21%Ptr2%L2           .NE.   Arg22%Ptr2%L2           ) ERROR STOP 37

    CLASS DEFAULT
      STOP 39
    END SELECT

  END IF
  END IF

  IF( PRESENT(Arg31) ) THEN
  IF( ALLOCATED(Arg31) ) THEN

    IF ( Arg31%K0   .NE.   Arg32%K0     ) ERROR STOP 41
    IF ( Arg31%L0   .NE.   Arg32%L0     ) ERROR STOP 42
    IF ( Arg31%K1   .NE.   Arg32%K1     ) ERROR STOP 43
    IF ( Arg31%L1   .NE.   Arg32%L1     ) ERROR STOP 44
    IF ( Arg31%K2   .NE.   Arg32%K2     ) ERROR STOP 45
    IF ( Arg31%L2   .NE.   Arg32%L2     ) ERROR STOP 46

    IF ( Arg31%C1   .NE.   Arg32%C1     ) ERROR STOP 47
    IF ( Arg31%C2   .NE.   Arg32%C2     ) ERROR STOP 48
    IF ( Arg31%I    .NE.   Arg32%I      ) ERROR STOP 49
    IF ( Arg31%R    .NE.   Arg32%R      ) ERROR STOP 50
    IF ( Arg31%L    .nEQV.  Arg32%L      ) ERROR STOP 51
    IF ( Arg31%Z    .NE.   Arg32%Z      ) ERROR STOP 52
    IF ( Arg31%T0%K0.NE.   Arg32%T0%K0  ) ERROR STOP 53
    IF ( Arg31%T0%L0.NE.   Arg32%T0%L0  ) ERROR STOP 54

    IF ( ASSOCIATED( Arg31%Ptr2) .nEQV. ASSOCIATED( Arg32%Ptr2)  ) ERROR STOP 55
    IF ( Arg31%Ptr2%K2           .NE.   Arg32%Ptr2%K2           ) ERROR STOP 56
    IF ( Arg31%Ptr2%L2           .NE.   Arg32%Ptr2%L2           ) ERROR STOP 37

  END IF
  END IF

  END SUBROUTINE

  END MODULE

  PROGRAM dtpObjDecAttr17
  USE M

  TYPE(DT0(8,:)),         ALLOCATABLE :: T11
  CLASS(DT1(8,3,8,:)),    ALLOCATABLE :: T21
  TYPE(DT2(8,:,8,3,8,:)), ALLOCATABLE :: T31
  TYPE(DT0(8,3))         :: T12
  TYPE(DT2(8,3,8,3,8,3)) :: T22
  TYPE(DT2(8,3,8,3,8,3)), TARGET :: T32

  T32%C1 = "XYZ"
  T32%C2 = "ZYX"
  T32%I = 1234
  T32%R = 4321.
  T32%L = .TRUE.
  T32%Z = (1.,-1.)
  T32%Ptr2 => T32

  T22 = T32

  T11 = T12
  allocate (T21, source = T22)
  T31 = T32

  CALL ModSub(Arg11=T11, Arg12=T12, Arg21=T21, Arg22=T22, Arg31=T31, Arg32=T32)
  CALL ModSub(Arg12=T12, Arg21=T21, Arg22=T22, Arg31=T31, Arg32=T32)
  CALL ModSub(Arg21=T21, Arg22=T22, Arg31=T31, Arg32=T32)
  CALL ModSub(Arg22=T22, Arg31=T31, Arg32=T32)
  CALL ModSub(Arg31=T31, Arg32=T32)
  CALL ModSub(Arg32=T32)
  CALL T32%proc()
  CALL ModSub()

  CALL T21%proc(Arg11=T11, Arg12=T12, Arg21=T21, Arg22=T22, Arg31=T31, Arg32=T32)
  CALL T22%proc(Arg11=T11, Arg12=T12, Arg21=T21, arg22=t22)
  CALL T31%proc(Arg11=T11, Arg12=T12)
  CALL T32%proc()

  CALL ModSub(Arg11=T11, Arg12=T12, Arg22=T22, Arg31=T31, Arg32=T32)

  END



