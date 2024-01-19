      TYPE Base (l1)
        INTEGER, LEN  :: l1

        CHARACTER(l1), ALLOCATABLE :: C0
      END TYPE

      CONTAINS

      FUNCTION func(b2)
        CLASS(Base(*)), INTENT(IN) :: b2
        CLASS(Base(b2%l1)), ALLOCATABLE :: func

            func%C0 = b2%C0

      END FUNCTION
END
