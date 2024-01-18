      TYPE Node  (l1)
        INTEGER, LEN :: l1

        CHARACTER(l1), ALLOCATABLE :: tag(:)
      END TYPE Node

      SELECT TYPE ( A => foo( Node(10)([( CHAR(I), I = 48, 50)]) ) )
        CLASS IS (Node(*))
           print*, A%tag(1)
           print*, A%tag

        CLASS DEFAULT
           STOP 21

      END SELECT

      CONTAINS

      FUNCTION foo(Obj)
      CLASS(*), POINTER  :: foo
      CLASS(Node(10)) :: Obj

         ALLOCATE (foo, source= Obj)
      END FUNCTION foo
END
