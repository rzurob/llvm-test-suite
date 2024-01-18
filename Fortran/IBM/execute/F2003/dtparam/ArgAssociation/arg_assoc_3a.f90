!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 20, 2009
!*  PRIMARY FUNCTIONS TESTED   : See Description below.
!*  REFERENCE                  : Feature Number 361232
!*
!*  DESCRIPTION:
!*
!*  12.4.1 1 Actual arguments, dummy arguments, and argument association
!*
!*  In a reference to a type-bound procedure that has a passed-object dummy
!*  argument (4.5.3.3), the data-ref of the function-reference or call-stmt is
!*  associated, as an actual argument, with the passed-object dummy argument.
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module m
    type base(n)
        integer, len :: n
        integer :: arr(n)
        procedure(pinterface), pointer, pass(this) :: proc
    end type

    interface
        subroutine pinterface ( dt, this )
            import
            class(base(*)) :: this
            type(base(this%n)) :: dt
        end subroutine
    end interface

    contains
        subroutine sub1 ( dt, this )
            class(base(*)) :: this
            type(base(this%n)) :: dt
            print *,'in sub1:'
            print *,'this%n=',this%n
            print *,'dt%n=',dt%n
            print *,'this%arr=',this%arr
            print *,'dt%arr=',dt%arr
        end subroutine
end module

use m
type(base(:)), allocatable :: a
allocate(base(4) :: a)

a%arr = [1,2,3,4]
a%proc => sub1

call a%proc(a)
end
