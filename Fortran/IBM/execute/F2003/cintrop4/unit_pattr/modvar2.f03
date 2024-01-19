!*********************************************************************
!***********************************************************************

      module modvar2
        integer, bind(c) :: x = 1

        real      :: y = 2.0
        character :: ch /'%'/
        bind(c) y, ch

        complex(4) :: cmp = (1.2, 3.4)
        integer    :: ar(3,2,1) = 3
        bind(c) :: cmp, ar
      end module

      program p1
        use modvar2

        print *, x, y, ch
        print *, cmp
        print *, ar
      end
