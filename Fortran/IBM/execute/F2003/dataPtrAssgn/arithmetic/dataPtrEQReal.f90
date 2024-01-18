!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - lb/ub of data-ptr are renamed local vars, data-ptr is ext function name
!* - data-tar is an internal function name of the ext function
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
    integer :: var1, var2

    interface test
        function func()
            class(*), pointer :: func(:)
        end function
    end interface

end module

program main
    use m

    var1 = 128
    var2 = 255

    select type(x => test())
        type is (real)
            if ( .not. all(x == (/(real(i,4),i=1,128)/) )) stop 5
        class default
            stop 4
    end select

end program

function func()
    use m, only:var1,var2, lb => var1, ub=> var2
    class(*), pointer :: func(:)

    func(lb:ub) => get_val(var1,var2)

    if ( .not. associated(func)) stop 1
    if ( lbound(func, 1) /= 128 ) stop 2
    if ( ubound(func, 1) /= 255 ) stop 3

    contains
        function get_val(a1, a2)
            integer a1, a2
            real, pointer :: get_val(:)

            allocate(get_val(a2-a1+1), source=(/(real(i,4),i=1,a2-a1+1)/) )
        end function
end function
