!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : ArrayConstructorTypeSpec08lk
!*
!*                               ArrayConstructorTypeSpec08 by Rob James)
!*  DATE                       : 2008-01-29 (original: 09/15/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DESCRIPTION                : Testing array constructors with type
!*                               specifications as actual arguments.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      type t (lt_1) ! lt_1=4
         integer, len :: lt_1
        integer i
      end type
      type, extends(t) :: t2 (kt2_1) ! kt2_1=4
         integer, kind :: kt2_1
        integer(kt2_1) j
      end type
      call sub((/x()/))
      contains
        function x()
          class(t(:)), allocatable :: x ! tcx: (:)
          allocate(x, source=t2(4,4)(5,10)) ! tcx: (4,4)
        end function
        subroutine sub(a)
          class(t(4)) :: a(:) ! tcx: (4)
          select type(a)
            type is (t(*)) ! tcx: (*)
              stop 1
            type is (t2(*,4)) ! tcx: (*,4)
              if (size(a) /= 1) stop 2
              if (a(1)%i /= 5) stop 3
              if (a(1)%j /= 10) stop 4
            class default
              stop 5
          end select
        end subroutine
      end


! Extensions to introduce derived type parameters:
! type: t - added parameters (lt_1) to invoke with (4)/declare with (*) - 3 changes
! type: t2 - added parameters (kt2_1) to invoke with (4,4)/declare with (4,4) - 2 changes
