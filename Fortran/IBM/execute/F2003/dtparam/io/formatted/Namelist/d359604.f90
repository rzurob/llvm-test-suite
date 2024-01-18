      TYPE Base (l1)
        INTEGER, LEN  :: l1

        CHARACTER(l1)  :: Carr(l1)
      END TYPE Base

       CLASS(Base(10)), ALLOCATABLE :: d1

       ALLOCATE(Base(10):: d1 )

       call sub1(d1)

      CONTAINS

      SUBROUTINE sub1(arg1)
        TYPE(Base(*))  :: arg1

       NAMELIST /NML1/arg1

       OPEN (1, file = 'd359604.In', form='formatted')

       READ(1, NML=NML1)

      END SUBROUTINE sub1

END
