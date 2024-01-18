!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration2
! PROGRAMMER                 : Yong Du
! DATE                       : 06/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The procedure is a dummy argument and has
!                              the SAVE attribute. Then it must be
!                              declared with the POINTER attribute.
!                              Poly and unlimited poly.
!
!                              This test case is diagnostic.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type Base
        integer i
    end type

    type, extends(Base) :: Child
        integer j
    end type

    contains

    subroutine sub1(b, p)
        class(Base), pointer, intent(in) :: b
        procedure(integer), SAVE :: p
        print *, "sub1"
    end subroutine

    integer function func1(b, p)
        class(*), pointer, intent(in) :: b
        procedure(integer), SAVE :: p
        func1 = 10
    end function
end module

program declaration002
end
