! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/F2003/allocEnh/construct/selectType002.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

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
!*  DATE                       : 09/18/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test the component of the associate name appears
!                               as the variable in the intrinsic assignment.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind            :: k1
        integer(k1), allocatable :: id
    end type

    type, extends(base) :: child    ! (8)
        real(k1), allocatable :: data(:)
    end type

    type container(k2,k3)    ! (4,8)
        integer, kind                :: k2,k3
        class(base(k3)), allocatable :: data
    end type

    contains

    subroutine printBase (b)
        class(base(8)), intent(in), allocatable :: b

        if (allocated(b)) then
            select type (b)
                type is (base(8))
                    if (allocated(b%id)) print *, b%id

                class is (child(8))
                    if (allocated(b%id)) write (*, '(i10,1x)', advance='no') b%id

                    if (allocated(b%data)) then
                        write (*, '(a, i10, 1x, i10, 1x)', advance='no') &
                            'bounds: ', lbound(b%data), ubound(b%data)

                        write (*, '(10(g15.8, 1x))') b%data

                    else
                        print *, 'data not allocated'
                    end if

                class default
                    error stop 10_4
            end select
        end if
    end subroutine

end module

program selectType002
use m
    class (container(4,8)), pointer :: co1(:)
    double precision d1(0:99)

    allocate (co1(10))

    d1 = log((/(i*1.0d2, i=1,100)/))

    !! first block is to allocate components
    select type (co1)
        type is (container(4,8))
            do i = 1, 10, 2
                allocate (co1(i)%data)
                allocate (child(8) :: co1(i+1)%data)
            end do

        class default
            error stop 1_4

    end select

    !! select block is to set the components' values
    do i = 1, 10
        select type (x => co1(i)%data)
            class is (base(8))
                x%id = 2**i

            type is (child(8))
                x%id = i
                x%data = d1(0:i**2-1:i)

            class default
                error stop 2_4
        end select
    end do

    do i = 1, 10
        call printBase (co1(i)%data)
    end do
end
