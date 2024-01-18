    ! FUNC: procptr pointer associated with an external function.
    module mmm

    TYPE :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L
    END TYPE

    TYPE, EXTENDS(DT) :: DT1
      INTEGER(K)   :: I=K
      CHARACTER(L) :: C="!!!!"
    END TYPE

    end module

   

    use mmm
      interface
        FUNCTION extFun(Arg)
          import
          CLASS(DT1(4,*)):: Arg
          type(dt1(4,4)) :: extFun
        END FUNCTION
      end interface
    PROCEDURE(IntFun), POINTER :: ProcPtr => NULL() 

    type(dt1(4,4)) :: dtobj
    type(dt1(4,5)) :: arg1 

    procptr => extfun

    dtobj = procptr(arg1)
    contains
      FUNCTION IntFun(Arg)
        CLASS(DT1(4,*)):: Arg
        type(dt1(4,4)) :: IntFun
      END FUNCTION

    END

    FUNCTION extFun(Arg)
      use mmm
      CLASS(DT1(4,*)):: Arg
      type(dt1(4,4)) :: extFun
      print*, arg%k
      print*, arg%l
    END FUNCTION
