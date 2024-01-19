!***********************************************************************
!* =====================================================================
!*
!*                               ArrayConstructorTypeSpec11 by Rob James)
!*  DATE                       : 2008-01-29 (original: 09/15/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing array constructors with type
!*                               specifications as actual arguments.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      type t
        integer i
      end type
      type, extends(t) :: t2 (lt2_1) ! lt2_1=4
         integer, len :: lt2_1
        integer j
      end type
      call sub([t::x()])
      contains
        function x()
          class(t), pointer :: x
          allocate(x, source=t2(4)(5,10)) ! tcx: (4)
        end function
        subroutine sub(a)
          class(t) :: a(:)
          select type(a)
            type is (t)
              if (a(1)%i /= 5) error stop 1
              if (size(a) /= 1) error stop 2
            type is (t2(*)) ! tcx: (*)
              stop 3
            class default
              stop 4
          end select
        end subroutine
      end


! Extensions to introduce derived type parameters:
! type: t2 - added parameters (lt2_1) to invoke with (4)/declare with (*) - 2 changes
