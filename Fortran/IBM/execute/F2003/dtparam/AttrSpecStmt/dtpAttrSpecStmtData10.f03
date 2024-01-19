!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 20, 2007
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
!*  -- DATA statement
!*
!*  the named constant or derived type accessible by use association.
!*
!*  (ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    CONTAINS
    PROCEDURE :: ModFun0
  END TYPE

  TYPE,  EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND :: K1=1
    INTEGER(K0), LEN  :: L1=1
    INTEGER(K1)       :: R(L1)!=K1
    CONTAINS
    PROCEDURE :: ModFun1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND :: K2=1
    INTEGER(K1), LEN  :: L2=1
    INTEGER(K2)   :: I(L2)!=K2
    CHARACTER(L2) :: C(L2)!=CHAR(K2)
    TYPE(DT2(K0,L0,K1,L0,K2,L2)), POINTER :: Ptr!=>NULL()
    CONTAINS
    PROCEDURE :: ModFun2
  END TYPE

  TYPE(DT0(1,3)),         PARAMETER :: CT0=DT0(1,3)()
  TYPE(DT1(1,3,4,5)),     PARAMETER :: CT1=DT1(1,3,4,5)(DT0=DT0(1,3)(),R=[1,2,3,4,5])
  TYPE(DT2(1,3,4,5,8,7)), PARAMETER :: CT2=DT2(1,3,4,5,8,7)(                   &
                                               DT1=DT1(1,3,4,5)([1,2,3,4,5]),  &
                                                 I=[1,2,3,4,5,6,7],            &
                                                 C=CHAR([1,2,3,4,5,6,7]),      &
                                               Ptr=NULL() )

  INTEGER, PARAMETER  :: N=96

  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,*)), INTENT(IN) :: Arg
  INTEGER ModFun0
    ModFun0 = Arg%L0
  END FUNCTION

  FUNCTION ModFun1(Arg)
  CLASS(DT1(1,*,4,*)), INTENT(IN) :: Arg
  COMPLEX ::  ModFun1(2)
    ModFun1(1) =  (Arg%K0, Arg%L0)
    ModFun1(2) =  (Arg%K1, Arg%L1)
  END FUNCTION

  FUNCTION ModFun2(Arg)
  CLASS(DT2(1,*,4,*,8,*)), INTENT(IN) :: Arg
  integer modfun2(size(arg%i))
    ModFun2 = -Arg%I
  END FUNCTION

  END MODULE


  PROGRAM dtpAttrSpecStmtData10
  USE M, ONLY:N, C0=>CT0,C1=>CT1,C2=>CT2,D0=>DT0,D1=>DT1,D2=>DT2, modfun1

  INTEGER :: I,J

  TYPE(D0(1,3))         :: T0(N,N), T01(N,N)
  TYPE(D1(1,3,4,5))     :: T1(N,N)
  TYPE(D2(1,3,4,5,8,7)) :: T2(N,N)
  TYPE(D0(1,3))         :: S0(N,N)
  TYPE(D1(1,3,4,5))     :: S1(N,N)
  TYPE(D2(1,3,4,5,8,7)) :: S2(N,N)
  INTEGER, PARAMETER     :: NN =  N*N
  INTEGER, PARAMETER     :: NN5=5*N*N
  INTEGER, PARAMETER     :: NN7=7*N*N

  DATA    ((T0(I,J),     J=1,N), I=1,N )   / NN*C2%DT0  /
  DATA    (((T1(I,J)%R(k), k=1,5),   J=1,N), I=1,N )   / NN5*C1%R(1) /
  DATA    ((T2(I,J)%DT1, J=1,N), I=1,N )   / NN *C2%dT1  /
  DATA    (((T2(I,J)%I(k),k=1,7),   J=1,N), I=1,N )   / NN7*C2%I(1) /
  DATA    (((T2(I,J)%C(k),k=1,7),   J=1,N), I=1,N )   / NN7*C2%C(1) /
  DATA    ((T2(I,J)%Ptr, J=1,N), I=1,N )   / NN *NULL()   /

  DATA    ((S0(I,J),     J=1,N), I=1,N )   / NN *D0(1,3)()  /
  DATA    ((S1(I,J),     J=1,N), I=1,N )   / NN *D1(1,3,4,5)(DT0=D0(1,3)(), R=[1,2,3,4,5]) /
  DATA    ((S2(I,J),     J=1,N), I=1,N )   / NN *D2(1,3,4,5,8,7)(                     &
                                                     DT1=D1(1,3,4,5)(R=[1,2,3,4,5]),  &
                                                       I=[1,2,3,4,5,6,7],             &
                                                       C=CHAR([1,2,3,4,5,6,7]),       &
                                                     Ptr=NULL() )  /

  INTEGER :: IArr(7)=0


  DO I=1, N
  DO J=1, N

    IF ( T0(I,J)%ModFun0()              .NE. T0(I,J)%L0    ) ERROR STOP 9
    IF ( ANY( T1(I,J)%R                      .NE. C1%R(1)       ) ) ERROR STOP 10
    IF ( ANY( T1(I,J)%ModFun1() .NE. [(1,3),(4,5)] ) ) ERROR STOP 11

    IF ( ANY( T2(I,J)%R .NE. [1,2,3,4,5]           ) ) ERROR STOP 12
    IF ( ANY( T2(I,J)%I .NE. C2%I(1)               ) ) ERROR STOP 13
    IF ( ANY( T2(I,J)%C .NE. CHAR(1)               ) ) ERROR STOP 14
    IF ( ASSOCIATED(T2(I,J)%Ptr ) )               ERROR STOP 15

    IArr = T2(I,J)%ModFun2()
    IF ( ANY( IArr .NE. -1   ) ) ERROR STOP 16
    IF ( ANY( T2(I,J)%DT1%ModFun1() .NE. [(1,3),(4,5)] ) ) ERROR STOP 17

    IF ( S0(I,J)%ModFun0()              .NE. S0(I,J)%L0    ) ERROR STOP 19
    IF ( ANY( S1(I,J)%R                      .NE. C1%R          ) ) ERROR STOP 20
    IF ( ANY( S1(I,J)%ModFun1() .NE. [(1,3),(4,5)] ) ) ERROR STOP 21

    IF ( ANY( S2(I,J)%R .NE. [1,2,3,4,5]           ) ) ERROR STOP 22
    IF ( ANY( S2(I,J)%I .NE. C2%I                  ) ) ERROR STOP 23
    IF ( ANY( S2(I,J)%C .NE. CHAR([1,2,3,4,5,6,7]) ) ) ERROR STOP 24
    IF (  ASSOCIATED(S2(I,J)%Ptr) )                ERROR STOP 25

    IArr = S2(I,J)%ModFun2()
    IF ( ANY( IArr .NE. [-1,-2,-3,-4,-5,-6,-7]   ) ) ERROR STOP 26
    IF ( ANY( ModFun1(D1(1,-3,4,-5)(0)) .NE. [(1,-3),(4,-5)] ) ) ERROR STOP 27

  END DO
  END DO

  END

