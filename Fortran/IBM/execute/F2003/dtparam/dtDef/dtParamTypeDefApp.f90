!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefApp
!*
!*  DATE                       : Dec. 01, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type definition
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
!*  Use of parameterized types
!*
!*  (339738)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDefApp

    TYPE :: DT0(K,L)
      INTEGER, KIND :: K = 4
      INTEGER, LEN  :: L = 4
    END TYPE

    TYPE, EXTENDS(DT0) :: DT1(K1,L1)
      INTEGER, KIND    :: K1 = 4
      INTEGER, LEN     :: L1 = 4
      INTEGER(K)       :: I(K) = 1
      REAL(K1)         :: R(K) = 1.0
      CHARACTER(K1+K)  :: C(K) = "abc"
      COMPLEX(K)       :: Cplx(K) = (1.0, -1.0)
      LOGICAL          :: LL(K)   = .TRUE.
      TYPE(DT0(K1))    :: VDT0(K) = DT0(4,4)()
      TYPE(DT0(K1, :)), POINTER           :: PDT0=>NULL()
      TYPE(DT0(K1, :)), ALLOCATABLE       :: ADT0
      PROCEDURE(INTEGER), NOPASS, POINTER :: ProcPtr=>NULL()

      CONTAINS
      PROCEDURE, NOPASS :: ExtFuN

    END TYPE DT1

    INTERFACE

      FUNCTION ExtFun(Arg)
      INTEGER :: Arg, ExtFun
      END FUNCTION

    END INTERFACE

    TYPE(DT1(4,4,4,4)) :: T
    TYPE(DT0(4,4)), TARGET :: TarT

    T=DT1(K=T%K, L=T%L, K1=T%K1, L1=T%L1)(  &
        &   I=-1,            &
        &   R=-1.,           &
        &   C="cba",         &
        &   Cplx=(-1.,1.),   &
        &   LL=.FALSE.,      &
        &   VDT0= DT0(4,4)(),&
        &   PDT0=TarT,       &
        &   ADT0=DT0(4,4)(), &
        &   ProcPtr=ExtFun   )


    IF ( T%K            .NE.   4          ) STOP 11
    IF ( T%L            .NE.   4          ) STOP 12
    IF ( T%K1           .NE.   4          ) STOP 13
    IF ( T%L1           .NE.   4          ) STOP 14
    IF ( ANY( T%I       .NE.   -1       ) ) STOP 15
    IF ( ANY( T%R       .NE.   -1.0     ) ) STOP 16

    DO I=1, 4
      IF ( TRIM(T%C(I)) .NE.   "cba"      ) STOP 17
    END DO

    IF ( ANY( T%Cplx    .NE.   (-1.,1.) ) ) STOP 18
    IF ( ANY( T%LL      .NEQV. .FALSE.  ) ) STOP 19
    IF (  T%VDT0%K      .NE.   4          ) STOP 20
    IF (  T%VDT0%L      .NE.   4          ) STOP 21
    IF (  ASSOCIATED(T%PDT0, TarT)        ) STOP 22
    IF (  .NOT. ASSOCIATED(T%PDT0)        ) STOP 23  ! zero sized storage
    IF ( .NOT. ALLOCATED(T%ADT0)             ) STOP 24
    IF ( .NOT. ASSOCIATED(T%ProcPtr, ExtFun) ) STOP 25
    IF ( T%ProcPtr(-1) .NE. -1               ) STOP 26
    IF ( T%ExtFun(-1)  .NE. -1               ) STOP 27


  END

  FUNCTION ExtFun(Arg)
  INTEGER :: Arg, ExtFun
    ExtFun = Arg
  END FUNCTION

