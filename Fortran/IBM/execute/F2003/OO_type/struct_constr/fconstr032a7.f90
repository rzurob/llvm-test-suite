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
!*  DATE                       : 02/22/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : structure constructor (allocatable components'
!                               allocations in structure constructor; data
!                               source is array sections and pointer arrays)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer(8) id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type container
        class (child), allocatable :: data(:)
    end type
end module

program fconstr032a7
use m
    type (child), target :: c1(10)
    class (child), pointer :: c2(:)

    c1 = (/(child(i, name='test'), i=1, 10)/)
    
    c2 => c1(::3)

    associate (x => container (c1(2::2)))
        if (.not. allocated (x%data)) error stop 1_4

        if ((lbound(x%data,1) /= 1) .or. (ubound(x%data, 1) /= 5)) error stop 2_4

        if (any (x%data%id /= (/2,4,6,8,10/))) error stop 3_4
        if (any (x%data%name /= 'test')) error stop 4_4
    end associate

    associate (x => container (c2))
        if (.not. allocated (x%data)) error stop 5_4

        if ((lbound(x%data,1) /= 1) .or. (ubound(x%data,1) /= 4)) error stop 6_4

        if (any (x%data%id /= (/1, 4, 7, 10/))) error stop 7_4
        if (any (x%data%name /= 'test')) error stop 8_4
    end associate
end
