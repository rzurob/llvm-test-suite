  MODULE M
    TYPE :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I=K
      CHARACTER(L) :: C="!!!!!!!"
      PROCEDURE(IntFun), NOPASS, POINTER :: ProcPtr => NULL()
    END TYPE

  CONTAINS

    FUNCTION IntFun(Arg)
      type(DT1(8,*)):: Arg
      character(Arg%L) :: IntFun
      IntFun = Arg%C
    END FUNCTION

  END MODULE

  use m
  integer jjj
  type(dt1(8, 7)) :: dt1obj
  type(dt1(8, 7)) :: dt1obj2

  dt1obj%procptr => IntFun
  print*, dt1obj%procptr(dt1obj2)
  print*, sizeof(dt1obj%procptr(dt1obj2))
  end
