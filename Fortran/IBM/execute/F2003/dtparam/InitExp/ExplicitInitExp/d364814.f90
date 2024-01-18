      TYPE :: Base(l1)
        INTEGER, LEN  :: l1 = 1

        INTEGER   :: A0(l1) = -1
        CHARACTER(l1) :: C0 = 'Base'
      END TYPE

      TYPE, EXTENDS(Base)  :: Child

        CHARACTER(l1) :: C1 = 'Child'
      END TYPE

      TYPE(Child(3)) :: c1 = Child(3) ( C1 = 'ABC' )

      print*, c1%C1
      IF ( c1%C1 .NE. 'ABC' ) STOP 30
      CALL Check(c1)

      CONTAINS

      SUBROUTINE Check(Arg)
        CLASS(Child(*)) :: Arg

        print*, LEN(Arg%C1), Arg%l1
        print*, Arg%C1
        IF ( Arg%C1 .NE. 'ABC' ) STOP 31

      END SUBROUTINE
END
