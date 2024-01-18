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
!*  DATE                       : 01/20/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Test the PASS attribute; involve kind type
!                               parameter only.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        integer(k) :: id
        character(20) :: name
        procedure(genBase4), pointer :: p4 => null()
        procedure(genBase8), pointer :: p8 => null()
    end type

    interface genBase
        module procedure genBase4
        module procedure genBase8
    end interface

    contains

    function genBase4 (b)
        class (base(4)), intent(in) :: b
        type (base(4)) genBase4

        genBase4%id = b%id
        genBase4%name = b%name
        genBase4%p4 => b%p4
        nullify (genBase4%p8)
    end function

    function genBase8 (b)
        class(base(8)), intent(in) :: b

        type (base(8)) genBase8

        genBase8%id = b%id
        genBase8%name = b%name
        genBase8%p8 => b%p8
        nullify (genBase8%p4)
    end function
end module

program dtparamProcComp001
use m
    type (base(4)) :: b1, b3(10)
    type (base(8)) :: b2(100)

    b1%p4 => genBase4

    b1%id = 10
    b1%name = 'xlftest b1'

    b3(::2) = b1%p4()

    !!

    b2(1)%id = 2_8**32+ 100
    b2(1)%name = b1%name(1:8)//'b2(1)'

    b2(1)%p8 => genBase8

    b2(5) = b2(1)%p8()


    !! verify
    do i = 1, 10, 2
        if ((b3(i)%id /= 10) .or. (b3(i)%name /= 'xlftest b1')) error stop 1_4

        if (.not. associated(b3(i)%p4, genBase4)) error stop 2_4
    end do

    if ((b2(5)%id -100)/2**30 /= 4) error stop 3_4
    if (b2(5)%name /= 'xlftest b2(1)') error stop 4_4
end
