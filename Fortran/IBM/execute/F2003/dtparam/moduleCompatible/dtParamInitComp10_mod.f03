
  MODULE M

    TYPE :: DT(K, L)
      INTEGER(8), KIND :: K
      INTEGER(8), LEN  :: L
      INTEGER(K)    :: I(L)
      REAL(K)       :: R(L)
    END TYPE

    TYPE, EXTENDS(DT) :: DT1(K1,L1)
      INTEGER(1), KIND :: K1
      INTEGER(1), LEN  :: L1
      TYPE(DT(K1,:)), POINTER :: T2
      TYPE(DT(K1,:)), ALLOCATABLE :: T3

      TYPE(DT(4,4)) :: T
      TYPE(DT(K1,L1)) :: T1
    END TYPE

    contains

    function createObj ()
        type(DT1(4,4,4,4)) createObj

        createObj%i = 4
        createObj%r = 4.0
        nullify (createObj%t2)
        createObj%t1%i = 4
        createObj%t1%r = 4.0
    end function

    function createObj48 ()
        type(DT1(4,4,8,8)) createObj48

        createObj48%i = 4
        createObj48%r = 4.0
        nullify (createObj48%t2)
        createObj48%t1%i = 8
        createObj48%t1%r = 8.0
    end function

  END MODULE
