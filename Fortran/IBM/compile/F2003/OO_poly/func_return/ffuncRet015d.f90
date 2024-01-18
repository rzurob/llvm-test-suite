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
!*  DATE                       : 05/11/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly-function return (function results can NOT
!                               be an actual-arg to be associated with a
!                               dummy-arg of INTENT(INOUT) or INTENT(OUT)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
        integer, private :: id

        contains

        procedure :: makeData => makeBaseAlloc
    end type

    contains

    class(base) function makeBaseAlloc (b)
        allocatable makeBaseAlloc
        class(base), intent(in) :: b

        allocate (makeBaseAlloc, source=b)
    end function

    subroutine test1 (b1, b2)
        class(base), allocatable, intent(inout) :: b1
        class(base), allocatable, intent(out) :: b2
    end subroutine
end module

program ffuncRet014d
use m
    class(base), pointer :: b1
    class(base), allocatable :: b2

    allocate (b1)

    call test1 (b1%makeData(), b2)   !<-- illegal
    call test1 (b2, b1%makeData())   !<-- illegal
end
