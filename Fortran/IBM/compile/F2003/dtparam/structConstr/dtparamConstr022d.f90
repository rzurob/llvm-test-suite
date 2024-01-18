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
!*  DATE                       : 03/06/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Data-target is of differnt rank from the
!                               pointer component.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k,n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data
        integer(k/2) :: id
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name
        logical(k) :: flag
    end type

    type container (k)
        integer, kind :: k

        class(base(k,:)), pointer :: data(:)
    end type
end module

program dtparamConstr022d
use m
    type(child(8, 30, 32)), target :: c1(2,2)
    type(container(8)) :: co1

    co1 = container(8)(data=c1)
end
