! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet009a2.f
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
!*  DATE                       : 05/05/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly function return (defined unary operator -
!                               as a function returning poly allocatable array)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind         :: k1
        real(k1), allocatable :: r1(:)

        contains

        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface operator(-)
        module procedure minus
    end interface

    contains

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        if (allocated(b%r1)) then
            write (*, '(f10.2)') b%r1
        else
            print *, 'r1 not allocated'
        end if
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*)), intent(in) :: b

        if (allocated(b%r1)) then
            write (*, '(f10.2)', advance='no') b%r1
            write (*, *) new_line('a'), b%name
        else
            print *, '|, ', b%name
        end if
    end subroutine

    class(base(8)) function minus (b)
        class (base(8)), intent(in) :: b(:)
        allocatable minus(:)

        select type (b)
            type is (base(8))
                allocate (minus(size(b)))
            type is (child(8,1,*))
                allocate (minus(size(b)), source= &
                        (/(child(8,1,20)(null(), name='-'//b(i)%name), i=1, size(b))/))

            class default
                error stop 10_4
        end select

        call mapR1fromB2Minus

        contains

        subroutine mapR1fromB2Minus
            do i = 1, size(b)
                allocate (minus(i)%r1(lbound(b(i)%r1,1):ubound(b(i)%r1,1)), &
                            source=-b(i)%r1)
            end do
        end subroutine
    end function
end module

program ffuncRet009a2
use m
    class (base(8)), pointer :: b1(:), b2(:)

    real(8), parameter :: rp1(10) = (/(j*1.1, j= 1, 10)/)

    allocate (b1(10))

    do i = 1, 5
        allocate (b1(i)%r1(i), source=rp1(:i))
    end do

    do i = 6, 10
        allocate (b1(i)%r1(11-i), source=rp1(i:))
    end do

    !! test 1
    associate (x => (-b1))
        if (size(x) /= 10) error stop 1_4

        do i = 1, 10
            call x(i)%print
        end do
    end associate


    !! test 2
    allocate (b2(5), source=-b1(::2))

    do i = 1, 5
        call b2(i)%print
    end do

    !! test 3
    deallocate (b2)

    allocate (b2(5), source=-(-b1(6:)))

    do i = 1, 5
        call b2(i)%print
    end do
end
