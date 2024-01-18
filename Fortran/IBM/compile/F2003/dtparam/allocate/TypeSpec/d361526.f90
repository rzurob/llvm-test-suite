      TYPE Base  (l1)
        INTEGER, LEN  :: l1

        CHARACTER(l1) :: tag
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child (l2)
        INTEGER, LEN  :: l2

        CLASS(Base(l2)), ALLOCATABLE :: b_cmp
      END TYPE Child

      CLASS(Base(10)), ALLOCATABLE :: b1

      ALLOCATE(Child(10,18) :: b1)
      CALL alloc_comp(b1)

      CONTAINS
!*
      SUBROUTINE alloc_comp(Arg)
         CLASS(Base(*)), ALLOCATABLE :: Arg

         SELECT TYPE ( Arg )
              CLASS IS (Child(*,*))
                 !ALLOCATE(Base(Arg%l2) :: Arg%b_cmp)           !<---- use this line, tc passes
                 ALLOCATE(Child(Arg%l2,Arg%l2) :: Arg%b_cmp)    
         END SELECT

      END SUBROUTINE alloc_comp

END
