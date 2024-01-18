!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrEqvLog.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - data-ptr is a module function; data-tar is its internal function return
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    contains
        function func(arg)
            logical, pointer :: func(:)
            logical, target :: arg(:)

            func(ubound(arg,1):) => get_value(arg)

            if ( .not. associated(func)) stop 1
            if ( lbound(func,1) /= 10) stop 2
            if ( ubound(func,1) /= 19)  stop 3

            contains
                function get_value(arg)
                    logical, pointer :: get_value(:)
                    logical :: arg(:)

                    allocate(get_value(size(arg)), &
                        source=arg(ubound(arg,1):lbound(arg,1):-1))
                end function
        end function
end module

program main
    use m

    type A
        logical, pointer :: ptr(:)
    end type

    logical, target, allocatable :: tar(:)
    type(A) :: a1

    allocate(a1%ptr(10), source = (/( mod(i,3)==2, i=1,10) /))

    a1%ptr = func( a1%ptr )

    if ( .not. all(a1%ptr .eqv. (/.false., .false., .true., .false., .false., &
            .true., .false., .false., .true., .false. /))) stop 10

end program
