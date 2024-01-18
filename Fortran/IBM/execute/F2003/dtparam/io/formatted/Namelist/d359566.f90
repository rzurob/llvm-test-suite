      TYPE Node  (l1)
        INTEGER, LEN :: l1
        CHARACTER(l1) :: tag
      END TYPE Node

      TYPE, EXTENDS(Node) :: ExtNode
        CLASS(Node(l1)), POINTER :: Next
      END TYPE ExtNode

      TYPE(Node(10)), TARGET :: FirstNode = (Node(10) (tag = 'B'))

      SELECT TYPE (A => foo((ExtNode(10)(Node = Node(10) (tag = 'A'), Next=FirstNode))))

        CLASS IS (ExtNode(*))
             print*, A%Next%tag

      END SELECT

      CONTAINS

      FUNCTION foo(Obj)
      CLASS(*), POINTER  :: foo
      CLASS(ExtNode(10)) :: Obj

      ALLOCATE (foo, source= Obj)

      END FUNCTION foo

END
