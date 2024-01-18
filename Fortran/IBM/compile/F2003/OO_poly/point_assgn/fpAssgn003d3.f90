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
!*  DESCRIPTION                : data pointer assignment (C723, the 2nd part:
!                               the variable in the data pointer assignment
!                               should not be an array with vector subscript)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn003d3
    class (*), pointer :: x(:)

    integer*4, target :: Z(5, 7), U(3), V(4)

    u = (/1, 3, 2/)

    v = (/2, 1, 1, 3/)

    x => z(u, v)       !<-- illegal

    print *, associated (x, z(2, v)) !<-- illegal
end
