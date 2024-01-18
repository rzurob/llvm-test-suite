  ! DIAG: the KIND type parameter of the return of procptr is 8, 
  !       but the target function is 4 

  MODULE M
    TYPE :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I=K
      CHARACTER(L) :: C="!!!!!!!"
      PROCEDURE(Integer(k)), NOPASS, POINTER :: ProcPtr => NULL() 
    END TYPE

  END MODULE

  use m
  external func1
  integer func1
  type(dt1(8, 8)) :: dt1obj
  dt1obj%procptr => func1
  end
  
  integer function func1()
    func1 = 12
  end function

