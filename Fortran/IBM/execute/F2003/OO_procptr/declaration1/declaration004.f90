!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DESCRIPTION                : The procedure pointer is a dummy argument
!                              and has the OPTIONAL attribute. The
!                              containing procedure is a module
!                              function.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    integer function func1(p)
        procedure(integer), pointer, optional, intent(in) :: p
        if(present(p)) then
            if(associated(p)) then
                func1 = p()
            else
                func1 = 10
            endif
        else
            func1 = 10
        endif
    end function

    integer function func2()
        func2 = 20
    end function
end module

program declaration004
use m
    procedure(integer), pointer :: p
    p => func2

    print *, func1()
    print *, func1(p)
end
