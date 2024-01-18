!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 13, 2007
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
!*  -- TARGET statement
!*
!*    (337970)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    SEQUENCE
  END TYPE

  TYPE  :: DT1(K1, L1)
    INTEGER, KIND    :: K1=0
    INTEGER, LEN     :: L1=0
    SEQUENCE
    CHARACTER(L1+3)      :: C1 = "DT1"
  END TYPE

  TYPE :: DT2(K2,L2)
    INTEGER, KIND    :: K2
    INTEGER, LEN     :: L2
    SEQUENCE
    CHARACTER(L2)        :: C2=CHAR(K2)
    INTEGER(K2)          :: I=K2
    REAL   (K2)          :: R=K2
    LOGICAL(K2)          :: L=.TRUE._1
    COMPLEX(K2)          :: Z=CMPLX(K2, K2, K2)
    TYPE(DT0(K2, L2))    :: T0(L2)
    TYPE(DT2(K2, L2)), POINTER  :: Ptr
  END TYPE

  INTEGER,   PARAMETER   :: N=1024

  SAVE

  POINTER :: P0
  POINTER :: P1
  POINTER :: P2

  TYPE(DT0(1,3)) :: T0
  TYPE(DT1(4,5)) :: T1
  TYPE(Dt2(8,7)) :: T2

  TYPE(DT0(1,:)) :: P0(:)
  TYPE(DT1(4,:)) :: P1(:)
  TYPE(Dt2(8,:)) :: P2(:)

  TARGET T0(N)
  TARGET T1(N)
  TARGET T2(N)


  TYPE(DT2(8,7)), PARAMETER ::                     &
        CT =  DT2(8,7)   (                         &
                                  C2 = "ZYX",      &
                                   I = 1234,       &
                                   R = 4321.,      &
                                   L = .TRUE.,     &
                                   Z = (1.,-1.),   &
                                   Ptr  = NULL(),  &
                                   T0=DT0(8,7)() )


  END MODULE

  SUBROUTINE ExtSub(P0,P1,P2,T0,T1,T2)
  USE M, ONLY: DT0,DT1,DT2,CT,N

  DIMENSION   :: T0(N), T1(N), T2(N)
  DIMENSION   :: P0(:), P1(:), P2(:)

  TARGET      :: T0, T1, T2
  POINTER     :: P0, P1, P2

  TYPE(DT0(1,3)) :: T0
  TYPE(DT1(4,5)) :: T1
  TYPE(Dt2(8,7)) :: T2

  TYPE(DT0(1,:)) :: P0
  TYPE(DT1(4,:)) :: P1
  TYPE(Dt2(8,:)) :: P2

  P0(-N:) => T0
  P1(-N:) => T1
  P2(-N:) => T2

  END SUBROUTINE


  PROGRAM dtpAttrSpecStmtTar
  USE M
  IMPLICIT NONE
  INTEGER I

  INTERFACE
    SUBROUTINE ExtSub(P0,P1,P2,T0,T1,T2)
    USE M, ONLY: DT0,DT1,DT2,CT,N
    TYPE(DT0(1,3)), TARGET :: T0(N)
    TYPE(DT1(4,5)), TARGET :: T1(N)
    TYPE(Dt2(8,7)), TARGET :: T2(N)
    TYPE(DT0(1,:)), POINTER :: P0(:)
    TYPE(DT1(4,:)), POINTER :: P1(:)
    TYPE(Dt2(8,:)), POINTER :: P2(:)
    END SUBROUTINE
  END INTERFACE


  T0 = DT0(1,3)()
  T1 = DT1(4,5)(C1="XYZ")
  T2 = CT

  CALL ExtSub(P0,P1,P2,T0,T1,T2)

  IF ( .NOT. ASSOCIATED( P0     ) ) STOP 11
  IF ( .NOT. ASSOCIATED( P1, T1 ) ) STOP 12
  IF ( .NOT. ASSOCIATED( P2, T2 ) ) STOP 13

  IF ( SIZE( P0 ) .NE. N ) STOP 14
  IF ( SIZE( P1 ) .NE. N ) STOP 15
  IF ( SIZE( P2 ) .NE. N ) STOP 16

  IF ( LBOUND( P0,1 ) .NE. -N ) STOP 17
  IF ( LBOUND( P1,1 ) .NE. -N ) STOP 18
  IF ( LBOUND( P2,1 ) .NE. -N ) STOP 19

  DO I=-N, -1

    IF ( P0(I)%L0     .NE. 3         )  STOP 30
    IF ( P1(I)%L1     .NE. 5         )  STOP 32
    IF ( P2(I)%L2     .NE. 7         )  STOP 35

    IF ( P1(I)%C1     .NE. "XYZ"     )  STOP 40

    IF ( P2(I)%C2             .NE.   "ZYX"    )  STOP 52
    IF ( P2(I)%I              .NE.   1234     )  STOP 53
    IF ( P2(I)%R              .NE.   4321.    )  STOP 54
    IF ( P2(I)%L              .NEQV. .TRUE.   )  STOP 55
    IF ( P2(I)%Z              .NE.   (1.,-1.) )  STOP 56
    IF ( P2(I)%T0%K0          .NE.    8       )  STOP 57
    IF ( P2(I)%T0%L0          .NE.    7       )  STOP 61
    IF ( ASSOCIATED(P2(I)%Ptr).EQV.   .TRUE.  )  STOP 62
    IF ( SIZE(P2(I)%T0)       .NE.    7       )  STOP 63
    IF ( P2(I)%T0%K0          .NE.    8       )  STOP 64
    IF ( P2(I)%T0%L0          .NE.    7       )  STOP 65
  END DO

  END


