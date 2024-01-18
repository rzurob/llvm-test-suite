! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg009a4.f
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
!*  DATE                       : 05/18/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (VALUE attribute: the
!                               entity has the initial value of actual-arg)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind     :: k1
        real(k1), pointer :: data(:) => null()
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    interface printData
        module procedure printData
        module procedure printDataChild
    end interface

    contains

    subroutine printData (b)
        type(base(8)), value :: b

        if (associated(b%data)) write (*, '(5f10.2)') b%data
    end subroutine

    subroutine printDataChild (x)
        type(child(8,1,20)), value :: x

        if (associated(x%data)) then
            write (*, '(5f10.2)', advance='no') x%data
            write (*, *) '; ', x%name
        else
            write (*, *) x%name
        end if
    end subroutine


    subroutine test1 (x)
        class(*) :: x

        select type(x)
            class is (base(8))
                call printData (x)

            type is (child(8,1,*))
                call printData (x)

            class default
                print *, 'unknown type'
        end select
    end subroutine
end module

program fArg009a4
use m
    class (base(8)), allocatable :: b1, b2(:)

    !! test 1
    allocate (b1, source=child(8,1,20)(name='xlftest'))

    call test1(b1)

    !! test 2
    allocate (b1%data(2), source=(/1.5_8, 2.5_8/))

    call test1(b1)

    !! test 3

    allocate (b2(2))

    allocate (b2(2)%data(3), source=1.25_8)

    call test1 (b2(2))
    call test1 (b2(1))
end
