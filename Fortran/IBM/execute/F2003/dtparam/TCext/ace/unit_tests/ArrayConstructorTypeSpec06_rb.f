! GM DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/F2003/ace/unit_tests/ArrayConstructorTypeSpec06.f

!***********************************************************************
!* =====================================================================
!*
!*                               ArrayConstructorTypeSpec06 by Rob James)
!*  DATE                       : 2008-01-29 (original: 09/14/2006)
!*
!*  DESCRIPTION                : Testing array constructors with type
!*  specifications as actual arguments.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      type t(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
      end type
      type, extends(t) :: t2    ! (4)
        integer(k1) j
      end type
      class(t(4)), allocatable :: x
      allocate(x, source=t2(4)(5,10))
      call sub([t(4)::x])
      contains
        subroutine sub(a)
          class(t(4)) :: a(:)
          select type(a)
            type is (t(4))
              if (a(1)%i /= 5) stop 1
              if (size(a) /= 1) stop 2
            type is (t2(4))
              stop 3
            class default
              stop 4
          end select
        end subroutine
      end
