! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg041.f
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
!*  DATE                       : 05/31/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dummy_arg (recursive calls between two
!                               subroutines)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind            :: k1
        integer(k1), allocatable :: id
    end type

    integer, parameter :: recurseNo = 5000
    integer, save :: howManyCalls = 0


    contains

    recursive subroutine printX (x)
        implicit none
        class (*), intent(in) :: x

        select type (x)
            type is (integer)
                call printInt (x)
            type is (character(*))
                call printChar (x)
            type is (base(4))
                call printBase (x)
            class default
                print *, 'not previously defined types'
        end select
    end subroutine

    recursive subroutine printInt (i)
        implicit none
        integer, intent(in) :: i

        if (howManyCalls < recurseNo) then

            howManyCalls = howManyCalls + 1

            call printX (i)
        else
            print *, i
            howManyCalls = 0
        end if
    end subroutine

    recursive subroutine printChar (c)
        implicit none
        character(*), intent(in) :: c

        integer :: callNo = 0

        if (callNo < recurseNo) then
            callNo = callNo + 1

            call printX (c)
        else
            print *, c

            callNo = 0
        end if
    end subroutine

    recursive subroutine printBase (b)
        implicit none
        type (base(4)), intent(in) :: b

        integer, save :: callNo = 0
        type (base(4)) localTemp

        if (callNo < recurseNo) then
            callNo = callNo + 1

            if (allocated (b%id)) allocate (localTemp%id, source = b%id + 1)

            call printX (localTemp)
        else
            if (allocated(b%id)) print *, b%id

            callNo = 0
        end if
    end subroutine
end module

program fArg041
use m
    character(10), parameter :: c1 = 'test 01'

    class (base(4)), pointer :: b1

    allocate (b1, source=base(4)(200))

    call printX (100)

    call printX (c1)

    call printX (b1)

    call printX (base(4)(1))

    call printX (base(4)(null()))
end
