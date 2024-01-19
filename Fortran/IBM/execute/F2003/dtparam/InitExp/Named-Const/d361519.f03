      TYPE :: Base(l1)
        INTEGER, LEN :: l1

        CHARACTER(l1) :: tag = "Niels"
      END TYPE

      TYPE(Base(5)) :: b1, b2 = Base(5)( "Bohr" )

      print*, foo(b1)

      print*, foo(b2)

      CONTAINS

      FUNCTION foo(Arg)
        CLASS(Base(*)) :: Arg
        TYPE(Base(:)), ALLOCATABLE :: foo
          foo = Arg
      END FUNCTION
END
