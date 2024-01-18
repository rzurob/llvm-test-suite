      TYPE Base  (l1)
        INTEGER, LEN  :: l1

        INTEGER :: my_arr(l1)    
      END TYPE Base

      CLASS(Base(5)), ALLOCATABLE :: c1

      CALL allocate_me(c1)

      CONTAINS

      SUBROUTINE allocate_me(Arg)
      CLASS(Base(*)), ALLOCATABLE :: Arg

      ALLOCATE(Base(*) :: Arg)  

      END SUBROUTINE allocate_me
END

