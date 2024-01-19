      TYPE Base  (l1)
        INTEGER, LEN  :: l1
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        CLASS(Base(l2)), POINTER :: Cmp
      END TYPE Child

      TYPE(Child(10,10)) :: c1

      CALL sub2(c1)

      CONTAINS

      SUBROUTINE sub2(Arg)
      CLASS(Child(*,*)) :: Arg

        ALLOCATE(Base(*) :: Arg%Cmp)

      END SUBROUTINE sub2
END
