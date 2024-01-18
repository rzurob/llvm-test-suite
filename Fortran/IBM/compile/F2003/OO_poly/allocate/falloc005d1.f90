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
!*  DATE                       : 03/17/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : C631, source-expr must be of a type that the
!                               allocate object is type compatible with.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc005d1
use iso_c_binding
    type, bind(C) :: bType
        integer(C_INT) :: i1
    end type

    type (bType), target :: b1
    type (bType), pointer :: b2

    class (*), pointer :: x

    b1%i1 = 10

    x => b1
    
    allocate (b2, source=x)     !<-- illegal

    print *, b2

    b2 = x                      !<-- illegal

    print *, b2
end

