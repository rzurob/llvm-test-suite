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
!*  -- VALUE statement
!*
!*    (338007)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND    :: K1=K0
    INTEGER(K0), LEN     :: L1=K0
    CHARACTER(L1+3)      :: C1 = "DT1"
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=K1
    INTEGER(K1), LEN     :: L2=K1
    CHARACTER(L2)        :: C2=CHAR(K2)
    INTEGER(K2)          :: I=K2
    REAL   (K2)          :: R=K2
    LOGICAL(K2)          :: L=.TRUE._1
    COMPLEX(K2)          :: Z=CMPLX(K2, K2, K2)
    TYPE(DT0(K0, L0))    :: T0(L0)
    TYPE(DT2(K2, L2,k2,k2,k2,k2)), POINTER  :: Ptr
  END TYPE

  INTEGER,   PARAMETER   :: N=1024

  TYPE(DT0(2,3)) :: T0
  TYPE(DT1(2,3,4,5)) :: T1
  TYPE(DT2(2,3,4,5,8,7)) :: T2

  SAVE

  TYPE(DT2(2,3,4,5,8,7)), PARAMETER ::             &
        CT =  DT2(2,3,4,5,8,7)   (                 &
                                  C2 = "ZYX",      &
                                   I = 1234,       &
                                   R = 4321.,      &
                                   L = .TRUE.,     &
                                   Z = (1.,-1.),   &
                                   Ptr  = NULL(),  &
                                   T0=DT0(2,3)() )


  END MODULE

  SUBROUTINE ExtSub(T0,T1,T2)
  USE M, ONLY: DT0,DT1,DT2,CT,N

  VALUE      :: T0, T1, T2

  TYPE(DT0(2,3)) :: T0
  TYPE(DT1(2,3,4,5)) :: T1
  TYPE(Dt2(2,3,4,5,8,7)) :: T2

  T0 = CT%t0(1)
  T1 = CT%DT1
  T2 = CT

  END SUBROUTINE


  PROGRAM dtpAttrSpecStmtVal
  USE M
  IMPLICIT NONE
  INTEGER I

  INTERFACE
    SUBROUTINE ExtSub(T0,T1,T2)
    USE M, ONLY: DT0,DT1,DT2,CT,N
    TYPE(DT0(2,3)) :: T0
    TYPE(DT1(2,3,4,5)) :: T1
    TYPE(Dt2(2,3,4,5,8,7)) :: T2
    VALUE                  :: T0, T1, T2
    END SUBROUTINE
  END INTERFACE


  CALL ExtSub(T0,T1,T2)


    IF ( T0%L0     .NE. 3         )  STOP 30
    IF ( T1%L1     .NE. 5         )  STOP 32
    IF ( T2%L2     .NE. 7         )  STOP 35

    IF ( T1%C1     .NE. "DT1"     )  STOP 40

    IF ( T2%C2             .NE.   CHAR(8)  )  STOP 52
    IF ( T2%I              .NE.   8        )  STOP 53
    IF ( T2%R              .NE.   8.       )  STOP 54
    IF ( T2%L              .NEQV. .TRUE.   )  STOP 55
    IF ( T2%Z              .NE.   (8.,8.) )  STOP 56
    IF ( T2%T0%K0          .NE.    2       )  STOP 57
    IF ( T2%T0%L0          .NE.    3       )  STOP 61
    IF ( ASSOCIATED(T2%Ptr).EQV.   .TRUE.  )  STOP 62
    IF ( SIZE(T2%T0)       .NE.    3       )  STOP 63
    IF ( T2%T0%K0          .NE.    2       )  STOP 64
    IF ( T2%T0%L0          .NE.    3       )  STOP 65

  END


