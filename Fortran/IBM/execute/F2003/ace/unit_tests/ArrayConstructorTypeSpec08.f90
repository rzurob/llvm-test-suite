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
      call sub((/x()/))
      contains
        function x()
          class(t), allocatable :: x
          allocate(x, source=t2(5,10))
        end function
        subroutine sub(a)
          class(t) :: a(:)
          select type(a)
            type is (t)
              stop 1
            type is (t2)
              if (size(a) /= 1) stop 2
              if (a(1)%i /= 5) stop 3
              if (a(1)%j /= 10) stop 4
            class default
              stop 5
          end select
        end subroutine
      end
