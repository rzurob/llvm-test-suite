!***********************************************************************
!* =====================================================================
!* XL Fortran Test Case                            IBM INTERNAL USE ONLY
!* =====================================================================
!*
!*  TEST CASE NAME             : ArrayConstructorTypeSpec09k
!*
!*  PROGRAMMER                 : Glen Mateer (derived from
!*                               ArrayConstructorTypeSpec09 by Rob James)
!*  DATE                       : 2008-01-29 (original: 09/15/2006)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancements
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DRIVER STANZA              : xlf2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing array constructors with type
!*                               specifications as actual arguments.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

      type t (kt_1) ! kt_1=4
         integer, kind :: kt_1
        integer(kt_1) i
      end type
      type, extends(t) :: t2
        integer(4) j
      end type
      call sub([t(4)::x()]) ! tcx: (4)
      contains
        function x()
          class(t(4)), allocatable :: x ! tcx: (4)
          allocate(x, source=t2(4)(5,10)) ! tcx: (4)
        end function
        subroutine sub(a)
          class(t(4)) :: a(:) ! tcx: (4)
          select type(a)
            type is (t(4)) ! tcx: (4)
              if (a(1)%i /= 5) stop 1
              if (size(a) /= 1) stop 2
            type is (t2(4)) ! tcx: (4)
              stop 3
            class default
              stop 4
          end select
        end subroutine
      end


! Extensions to introduce derived type parameters:
! type: t - added parameters (kt_1) to invoke with (4)/declare with (4) - 4 changes
! type: t2 - added parameters () to invoke with (4)/declare with (4) - 2 changes
