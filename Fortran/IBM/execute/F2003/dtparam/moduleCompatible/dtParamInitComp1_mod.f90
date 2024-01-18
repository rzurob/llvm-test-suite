  MODULE M
    TYPE, ABSTRACT :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I(K)=K
      REAL(K)      :: R(K)=K
      COMPLEX(K)   :: Z(K)=(K,-K)
      CHARACTER(K) :: C(K) = CHAR(K)  !C(L) = CHAR(K)
      PROCEDURE(IntFun), POINTER :: ProcPtr => NULL() 
    CONTAINS
      PROCEDURE, PASS :: IntFun 
    END TYPE

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT1(4,*)):: Arg
    TYPE(DT1(4, arg%l)):: IntFun
      IntFun = Arg
    END FUNCTION

  END MODULE
