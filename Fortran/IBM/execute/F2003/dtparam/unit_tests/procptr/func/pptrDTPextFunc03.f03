  MODULE M
    TYPE :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I=K
      CHARACTER(L) :: C="!!!!!!!"
      PROCEDURE(Integer), NOPASS, POINTER :: ProcPtr => NULL()
    END TYPE
  END MODULE

  use m
  external func1
  integer func1
  type(dt1(4, 8)) :: dt1obj
  dt1obj%ProcPtr => func1
  print*, dt1obj%procptr()
  end

  integer function func1()
    func1 = 12
  end function
