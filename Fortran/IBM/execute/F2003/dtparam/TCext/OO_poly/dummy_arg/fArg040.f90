! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg040.f
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
!*  DATE                       : 05/26/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (array sections of the
!                               same array entities are used as the actual-arg;
!                               intented for testing optimizations)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind         :: k1
        real(k1), allocatable :: value(:)
        integer(k1)           :: id = -1

        contains

        procedure :: add => add2B
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: add => add2C
    end type

    contains

    real(8) function mergeArrays (r1, r2)
        allocatable mergeArrays(:)
        real(8), intent(in) :: r1(:), r2(:)

        integer sizes(2)

        sizes = (/size(r1), size(r2)/)

        if (sizes(1) >= sizes(2)) then
            allocate (mergeArrays(sizes(1)), source=r1)

            mergeArrays(:sizes(2)) = mergeArrays(:sizes(2)) + r2
        else
            allocate (mergeArrays(sizes(2)), source=r2)

            mergeArrays(:sizes(1)) = mergeArrays(:sizes(1)) + r1
        end if

    end function

    subroutine add2B (b1, b2)
        class (base(8)), intent(inout) :: b1
        class (base(8)), intent(in) :: b2

        b1%id = b1%id + b2%id

        if (allocated (b1%value)) then
            if (allocated (b2%value)) then
                associate (x => mergeArrays (b1%value, b2%value))
                    deallocate (b1%value)

                    allocate (b1%value(size(x)), source=x)
                end associate
            end if
        else
            if (allocated (b2%value)) then
                allocate (b1%value(size(b2%value)), source=b2%value)
            end if
        end if
    end subroutine


    subroutine add2C (b1, b2)
        class (child(8,1,*)), intent(inout) :: b1
        class (base(8)), intent(in) :: b2

        select type (b2)
            type is (child(8,1,*))
                call b1%base%add (b2%base)

                b1%name = trim (b1%name) // ' ' // trim (b2%name)
            class default
                error stop 15_4
        end select
    end subroutine

    subroutine addB (b, b1, b2)
        class (base(8)), intent(out) :: b(:)
        class (base(8)), intent(in) :: b1(size(b)), b2(size(b))

        if (.not. same_type_as (b1, b2)) error stop 10_4
        if (.not. same_type_as (b, b2)) error stop 11_4

        do i = 1, size(b)
            call b(i)%add (b1(i))

            call b(i)%add (b2(i))
        end do
    end subroutine
end module

program fArg040
use m
    class (base(8)), pointer :: b1(:)

    logical(4) precision_r8

    allocate (b1(1000), source=(/(child(8,1,20)(null(), i, 'test'), i=1,1000)/))

    do i = 1, 300
        !! set values for 1:300
        allocate (b1(i)%value(i), source=(/(j*1.0_8, j=1,i)/))

        !! set values for 301:900:2
        allocate (b1(2*i+299)%value(300), source=(/(/((300-j)*1.0_8, j=1,i)/), &
                    (/(300.0_8, k=1, 300-i)/)/))
    end do

    call addB (b1(302:900:2), b1(1:300), b1(301:899:2))


    !! verify the results
    do i = 302, 900, 2
        if (.not. allocated (b1(i)%value)) error stop 1_4

        if (size(b1(i)%value) /= 300) then
            print *, size(b1(i)%value)
            error stop 2_4
        end if

        if (b1(i)%id /= ((i-2) + (i-300)/2)) error stop 3_4

        do j = 1, 300
            if (.not. precision_r8 (b1(i)%value(j), 300.0_8)) error stop 4_4
        end do
    end do

    select type (x => b1(302:900:2))
        type is (child(8,1,*))
            if (any(x%name /= 'default test test')) error stop 5_4
        class default
            error stop 6_4
    end select
end
