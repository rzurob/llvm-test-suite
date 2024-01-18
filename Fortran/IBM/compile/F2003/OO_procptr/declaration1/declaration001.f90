!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The procedure is a dummy argument and has
!                              the INTENT attribute. Then it must be
!                              declared with the POINTER attribute.
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
    contains

    subroutine sub1(p)
        procedure(integer), intent(in) :: p
        print *, "sub1"
    end subroutine

    integer function func1(p)
        procedure(integer), intent(in) :: p
        func1 = 10
    end function
end module

program declaration001
end
