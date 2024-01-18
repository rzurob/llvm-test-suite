!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 01/11/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Deferred type-parameters in
!                               declaration-type-spec: function results.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (l)
        integer, len :: l

        character(l) :: name
        integer id
    end type

    interface
        function genBaseAlloc (name, id) result(res)
        import
            type (base(:)), allocatable :: res
            character(*), intent(in) :: name
            integer, intent(in) :: id
        end function
    end interface
end module

program deferdparamDTSpec006
use m
    type (base(10)) b1
    class(base(:)), allocatable :: b2

    b1 = genBaseAlloc ('xlftest 01', 100)

    allocate (b2, source=genBaseAlloc('abc', -1))

    if ((b1%id /= 100) .or. (b1%name /= 'xlftest 01')) error stop 1_4

    if ((b2%id /= -1) .or. (b2%name /= 'abc') .or. (b2%l /= 3)) error stop 2_4
end


function genBaseAlloc (name, id) result(res)
use m, only: base
    type (base(:)), allocatable :: res
    character(*), intent(in) :: name
    integer, intent(in) :: id

    allocate (base(len(name)):: res)

    res%name = name
    res%id = id
end function
