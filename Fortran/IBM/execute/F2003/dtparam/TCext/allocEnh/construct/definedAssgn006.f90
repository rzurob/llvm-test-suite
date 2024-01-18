! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/allocEnh/construct/definedAssgn006.f
! opt variations: -qnock -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
!                               Use a structure that contains two components,
!                               both having type-bound assignment defined.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type number(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: id

        contains

        procedure :: assgnID
        generic :: assignment(=) => assgnID
    end type

    type string(k2,n2)    ! (4,20)
        integer, kind :: k2
        integer, len  :: n2
        character(:), allocatable :: str

        contains

        procedure :: assgnStr
        generic :: assignment(=) => assgnStr
    end type

    contains

    elemental subroutine assgnID (a1, a2)
        class(number(*,4)), intent(out) :: a1
        class(number(*,4)), intent(in) :: a2

        if (allocated(a2%id)) a1%id = a2%id + 10000
    end subroutine

    elemental subroutine assgnStr (b1, b2)
        class(string(4,*)), intent(out) :: b1
        class(string(4,*)), intent(in) :: b2

        if (allocated(b2%str)) b1%str = ' ' // b2%str
    end subroutine
end module

module n

use m
    type employee(k3,n3,n4)    ! (4,20,2)
        integer, kind       :: k3
        integer, len        :: n3,n4
        type(number(n3,k3)) :: ID
        type(string(k3,n3)) :: name(n4)
    end type
end module

use n
    type(employee(4,:,:)), allocatable :: p1(:), p2
    type(number(20,4)) num1
    type(string(4,20)) name(2)

    num1%id = 1

    name(1)%str = 'Mr'
    name(2)%str = 'XYZ'

    p2 = employee(4,20,2)(num1, name)

    if ((.not. allocated(p2%id%id)) .or. &
        (.not. allocated(p2%name(1)%str)) .or. &
        (.not. allocated(p2%name(2)%str))) error stop 1_4


    if (p2%id%id /= 10001) error stop 2_4

    if ((len(p2%name(1)%str) /= 3) .or. &
        (len(p2%name(2)%str) /= 4)) error stop 3_4

    if ((p2%name(1)%str /= ' Mr') .or. &
        (p2%name(2)%str /= ' XYZ')) error stop 4_4
end
