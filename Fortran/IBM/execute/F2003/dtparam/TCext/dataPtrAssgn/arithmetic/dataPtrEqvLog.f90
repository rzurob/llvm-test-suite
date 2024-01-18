! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrEqvLog.f
! opt variations: -qnol

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrEqvLog.f
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
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

    type A(n1,k1)    ! (20,4)
        integer, kind        :: k1
        integer, len         :: n1
        logical(k1), pointer :: ptr(:)
    end type

    logical, target, allocatable :: tar(:)
    type(A(20,4)) :: a1

    allocate(a1%ptr(10), source = (/( mod(i,3)==2, i=1,10) /))

    a1%ptr = func( a1%ptr )

    if ( .not. all(a1%ptr .eqv. (/.false., .false., .true., .false., .false., &
            .true., .false., .false., .true., .false. /))) stop 10

end program
