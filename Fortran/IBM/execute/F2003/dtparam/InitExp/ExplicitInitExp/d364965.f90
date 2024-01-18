      TYPE Base (l1)
        INTEGER, LEN  :: l1

        INTEGER   :: A0(l1) = -1
      END TYPE

      TYPE(Base(3)) :: b1 = Base(3) (3)

      CALL CreateNewBase(b1)

      CONTAINS

      SUBROUTINE CreateNewBase(Arg)
        CLASS(Base(*)) :: Arg
        TYPE(Base(:)), ALLOCATABLE :: Obj

        Obj = Arg
        print*, Obj%A0
      END SUBROUTINE

END
