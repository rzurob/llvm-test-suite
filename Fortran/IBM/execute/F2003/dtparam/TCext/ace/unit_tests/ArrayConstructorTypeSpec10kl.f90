!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : ArrayConstructorTypeSpec10kl
!*
!*                               ArrayConstructorTypeSpec10 by Rob James)
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

      type t (kt_1,lt_1) ! kt_1,lt_1=4,1
         integer, kind :: kt_1
         integer, len :: lt_1
        integer(kt_1) i
      end type
      type, extends(t) :: t2
        integer(4) j
      end type
      call sub((/x()/))
      contains
        function x()
          class(t(4,:)), pointer :: x ! tcx: (4,:)
          allocate(x, source=t2(4,1)(5,10)) ! tcx: (4,1)
        end function
        subroutine sub(a)
          class(t(4,1)) :: a(:) ! tcx: (4,1)
          select type(a)
            type is (t(4,*)) ! tcx: (4,*)
              stop 1
            type is (t2(4,*)) ! tcx: (4,*)
              if (size(a) /= 1) stop 2
              if (a(1)%i /= 5) stop 3
              if (a(1)%j /= 10) stop 4
            class default
              stop 5
          end select
        end subroutine
      end


! Extensions to introduce derived type parameters:
! type: t - added parameters (kt_1,lt_1) to invoke with (4,1)/declare with (4,*) - 3 changes
! type: t2 - added parameters () to invoke with (4,1)/declare with (4,*) - 2 changes
