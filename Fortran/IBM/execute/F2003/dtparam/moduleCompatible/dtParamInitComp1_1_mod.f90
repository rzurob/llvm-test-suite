
  MODULE M

    INTEGER, PARAMETER :: N(128)=[(I, I=1, 128)]
    complex, PARAMETER :: ZN(128)=[((I,-I), I=1, 128)]

    TYPE, ABSTRACT :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I(K)=N(1:K)
      REAL(K)      :: R(K)=N(1:K)
      COMPLEX(K)   :: Z(K)=ZN(1:K)
      CHARACTER(L) :: C(K) = CHAR(K)  !C(L) = CHAR(K)
      PROCEDURE(IntFun), POINTER :: ProcPtr => NULL() 
    CONTAINS
      PROCEDURE, PASS :: IntFun 
    END TYPE

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT1(4,*)):: Arg
    TYPE(DT1(4, 4)):: IntFun
      IntFun = Arg
    END FUNCTION

  END MODULE
