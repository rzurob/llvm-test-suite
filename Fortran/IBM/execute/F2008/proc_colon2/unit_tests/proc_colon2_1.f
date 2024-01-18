! F2008: double colon separator is allowed in the module procedure
! and procedure statements in interface blocks.
! Test langlvl checking
!
      module m
        interface foo
          procedure x
          procedure :: y
          module procedure x2
          module procedure :: y2
        end interface
      contains
        subroutine x(a, b)
          integer :: a, b
        end subroutine
        subroutine y(c)
          real :: c
        end subroutine
        subroutine x2(d, e, f)
          character(10) d, e, f
        end subroutine
        subroutine y2(g)
          complex(16) g
        end subroutine
      end module
