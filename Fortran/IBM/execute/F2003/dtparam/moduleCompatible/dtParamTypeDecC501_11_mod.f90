
  MODULE M

  TYPE :: DT0(K1,K2,K4,K8,L0)
    INTEGER, LEN :: L0=0
    INTEGER(8), KIND :: K1 = 1
    INTEGER(4), KIND :: K2 = 2
    INTEGER(2), KIND :: K4 = 4
    INTEGER(1), KIND :: K8 = 8
  END TYPE

  CONTAINS

  PURE FUNCTION ModFun(Arg)
  INTEGER :: ModFun
  INTEGER, INTENT(IN) :: Arg
    ModFun = Arg
  END FUNCTION

  END MODULE

  MODULE M1
  USE M
  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    CHARACTER(LEN=L)  :: C
    TYPE(DT((((K))), L=((L)))),                   POINTER :: Ptr1
    TYPE(DT(K, (((L))+1))),     POINTER :: Ptr2    => NULL()
    TYPE(DT(K, ((L+2)))),       POINTER :: Ptr3(:) => NULL()
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(L)
  INTEGER :: L
  TYPE(DT(K=4, L=L)) :: T
  TYPE(DT(K=4, L=(((L+3))))), POINTER :: Ptr4(:)

  IF ( T%K        .NE.   4  )  STOP 11
  IF ( T%L        .NE.   2  )  STOP 12

  IF (  T%Ptr1%K   .NE.  4  )  STOP 21
  IF (  T%Ptr1%L   .NE.  2  )  STOP 22

  IF (  T%Ptr2%K   .NE.  4  )  STOP 31
  IF (  T%Ptr2%L   .NE.  3  )  STOP 32

  IF (  T%Ptr3%K   .NE.  4  )  STOP 41
  IF (  T%Ptr3%L   .NE.  4  )  STOP 42

  IF (    Ptr4%K   .NE.  4  )  STOP 51
  IF (    Ptr4%L   .NE.  5  )  STOP 52

  END SUBROUTINE

  END MODULE
