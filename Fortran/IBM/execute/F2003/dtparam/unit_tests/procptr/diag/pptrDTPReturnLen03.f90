  MODULE M
    TYPE :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I=K
      CHARACTER(L) :: C="!!!!!!!"
      PROCEDURE(type(dt(4, 7))), NOPASS, POINTER :: ProcPtr => NULL() 
    END TYPE

  END MODULE

  use m
  external func1
  type(dt(4, 8)) func1
  type(dt1(8, 8)) :: dt1obj
  type(dt(4, 7)) :: dttarget 
  dt1obj%procptr => func1
  dttarget = dt1obj%procptr()
  end
  
  type(dt(4,8)) function func1()
    use m
    integer iii
    integer jjj 
    iii = func1%k
    jjj = func1%l
  !  print*, iii
  !  print*, jjj 
  end function

