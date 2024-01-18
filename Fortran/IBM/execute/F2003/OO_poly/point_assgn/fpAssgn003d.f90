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
!*  DATE                       : 04/06/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : data pointer assignment (for the associated(),
!                               the TARGET used in this intrinsic has to be of a
!                               data that is allowed as if in a data pointer
!                               assignment)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
    end type

    type, extends(base) :: child
    end type
end module

program fpAssgn003d
use m

    class (base), pointer :: b_ptr
    class (child), pointer :: c_ptr

    type(child), target :: c = child()

    integer*4, target :: i1

    class (*), pointer :: x

    b_ptr => c
    c_ptr => c

    x => c

    print *, associated(b_ptr, x)   !<-- this is illegal
    print *, associated (b_ptr, i1) !<-- also illegal

    print *, associated (c_ptr, b_ptr)  !<-- also illegal
    print *, associated (c_ptr, c_ptr%base)  !<-- again illegal
end
