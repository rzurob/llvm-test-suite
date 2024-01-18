!=======================================================================
! XL Fortran Test Case                             IBM INTERNAL USE ONLY
!=======================================================================
! TEST BUCKET                : OO_procptr/declaration1
! PROGRAMMER                 : Yong Du
! DATE                       : 04/18/2005
! PRIMARY FUNCTIONS TESTED   : procedure pointer declaration
! DRIVER STANZA              : xlf90
! DESCRIPTION                : The procedure pointer is a dummy argument
!                              and has the OPTIONAL attribute. The
!                              containing procedure is an external
!                              function. The caller must has an explicit
!                              interface of the function.
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program declaration005
    interface
        integer function func1(p)
            procedure(integer), pointer, optional, intent(in) :: p
        end function

        integer function func2()
        end function
    end interface

    procedure(integer), pointer :: p
    p => func2

    print *, func1()
    print *, func1(p)
end

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
