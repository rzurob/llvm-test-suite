      ! reduced tc from defect 355488
      TYPE Base  (len1)
        INTEGER, LEN :: len1

        INTEGER ::  my_arr(len1)
      END TYPE Base

      TYPE, EXTENDS(Base) :: Child
        CLASS(Base(len1)), POINTER :: Cmp
      END TYPE Child

      CLASS(Child(:)), ALLOCATABLE :: dtv !<-- If len type param is specified, tc gives right results
      TYPE(Base(5)), TARGET :: tgt

      tgt%my_arr = 0

      ALLOCATE(Child(5):: dtv)
      print*, ALLOCATED(dtv)

      dtv%Cmp => tgt
      print*, ASSOCIATED(dtv%Cmp)

      END
