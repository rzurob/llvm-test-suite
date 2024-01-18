! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/allocEnh/construct/associate002.f
! opt variations: -qck -qnok -ql

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
!*  DATE                       : 09/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocatable enhancement
!                               Test that the associate name does not have
!                               allocatable attribute (and therefore no
!                               autoreallocation); use deferred char-length
!                               entities for the test.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        character(:), allocatable :: name
        character(:), allocatable :: names(:)
    end type
end module

program associate2
use m
    character(:), allocatable :: c1, c2(:), c3(:,:)

    type (base(4)) :: b1(10)

    c1 = 'xlftest'

    c2 = (/'ab', 'cd'/)

    allocate (character(10) :: c3(0:1,0:1))

    c3(:,:) = reshape((/'A', 'B', 'C', 'D'/), (/2,2/))

    associate (x => c1)
        x = 'abc '//c1
    end associate

    associate (x => c2)
        x = (/'a,b,c', 'c,d,e'/)
    end associate

    associate (x => c3)
        x = reshape((/c2, c2/), shape(x))
    end associate

    if ((len(c1) /= 7) .or. (c1 /= 'abc xlf')) error stop 1_4

    if ((len(c2) /= 2) .or. (size(c2) /= 2)) error stop 2_4

    if (any(c2 /= (/'a,', 'c,'/))) error stop 3_4

    if (any(lbound(c3) /= (/0,0/)) .or. any (ubound(c3) /= (/1,1/))) &
        error stop 4_4

    if (len (c3) /= 10) error stop 5_4

    if (any((/c3/) /= (/'a,', 'c,', 'a,', 'c,'/))) error stop 6_4

    !!test 2
    associate (x => b1)
        do i = lbound(x,1), ubound(x,1)
            x(i)%name = 'xlftest'//achar(i+65)

            x(i)%names = (/('abc', j=0,i)/)
        end do
    end associate

    do i = 1, 10
        if ((len(b1(i)%name) /= 8) .or. (len(b1(i)%names) /= 3)) error stop 7_4

        if (b1(i)%name /= 'xlftest'//achar(i+65)) error stop 8_4

        if (size(b1(i)%names) /= i+1) error stop 9_4

        if (any(b1(i)%names /= 'abc')) error stop 10_4
    end do

    associate (x => b1)
        do i = 1, 10
            associate (y => x(i)%name, z => x(i)%names)
                y = repeat('abc', i)

                z = (/('xyz',j=1,i+1)/)
            end associate
        end do
    end associate


    do i = 1, 10
        if ((len(b1(i)%name) /= 8) .or. (len(b1(i)%names) /= 3)) error stop 11_4

        if (b1(i)%name /= 'abcabcabc'(1:min(8,i*3))) error stop 12_4

        if (size(b1(i)%names) /= i+1) error stop 13_4

        if (any(b1(i)%names /= 'xyz')) error stop 14_4
    end do
end
