!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ArrayConstructorTypeSpec06.f
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : 09/14/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
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
      class(t), allocatable :: x
      allocate(x, source=t2(5,10))
      call sub([t::x])
      contains
        subroutine sub(a)
          class(t) :: a(:)
          select type(a)
            type is (t)
              if (a(1)%i /= 5) stop 1
              if (size(a) /= 1) stop 2
            type is (t2)
              stop 3
            class default
              stop 4
          end select
        end subroutine
      end
