!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The procedure is a dummy argument and has
!                              the SAVE attribute. Then it must be
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
        procedure(integer), SAVE :: p
        print *, "sub1"
    end subroutine

    integer function func1(p)
        procedure(integer), SAVE :: p
        func1 = 10
    end function
end module

program declaration002
end
