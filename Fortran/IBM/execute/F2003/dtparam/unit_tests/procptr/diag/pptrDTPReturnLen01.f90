  ! DIAG: the LENGTH type parameter of the return of procptr is 7,
  !       but the procptr target function func1 is 6.
  MODULE M
    TYPE :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I=K
      CHARACTER(L) :: C="!!!!!!!"
      PROCEDURE(character(L)), NOPASS, POINTER :: ProcPtr => NULL()
    END TYPE

  END MODULE

  use m
  external func1
  character(6) func1
  type(dt1(4, 7)) :: dt1obj
  dt1obj%procptr => func1
  end

  character(6) function func1()
    func1 = "Hello"
  end function

