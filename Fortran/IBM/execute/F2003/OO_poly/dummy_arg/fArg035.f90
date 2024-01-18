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
!*  DATE                       : 05/17/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : argument association (for allocatable dummy-arg
!                               the actual-arg is allowed to have an allocatio
!                               status of unallocated)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        class(*), pointer :: data(:) => null()
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    contains

    class(base) function copyData (b)
        class (base), allocatable, intent(in) :: b(:)
        allocatable copyData(:)

        if (allocated (b)) allocate (copyData(size(b)), source=b)
    end function
end module

program fArg035
use m
    class(base), allocatable :: b1(:)

    if (allocated (copyData(null(b1)))) error stop 1_4

    if (allocated (copyData(copyData(null(b1))))) error stop 2_4
    
    if (allocated (copyData(copyData(copyData(null(b1)))))) error stop 3_4

    if (allocated (copyData(copyData(copyData(copyData(null(b1))))))) &
                error stop 4_4
end
