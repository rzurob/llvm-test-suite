! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/15/2006
!*
!*  DESCRIPTION                : Testing array constructors with type
!*                               specifications as actual arguments.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type t
        integer i
      end type
      type, extends(t) :: t2
        integer j
      end type
      call sub([t::x()])
      contains
        function x()
          class(t), allocatable :: x
          allocate(x, source=t2(5,10))
        end function
        subroutine sub(a)
          class(t) :: a(:)
          select type(a)
            type is (t)
              if (a(1)%i /= 5) error stop 1
              if (size(a) /= 1) error stop 2
            type is (t2)
              stop 3
            class default
              stop 4
          end select
        end subroutine
      end