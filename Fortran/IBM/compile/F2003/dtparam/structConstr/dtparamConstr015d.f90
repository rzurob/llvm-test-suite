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
!*  DATE                       : 03/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: C486: Keyword missed for component-spec after
!                               the previous use of keyword.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    type, extends(base) :: child (l)
        integer, len :: l

        character(l) :: name
    end type
end module

program dtparamConstr015d
use m
    type (child(8, 10, 12)) :: c1
    type(child(4, :,:)), allocatable :: c2

    allocate (child(4,200, 20) :: c2)

    !! the following use of structure constructors are illegal
    c1 = child(8, 10, 12)(data=1.0d0, 'xlftest')

    c2 = child(4,200,20)(base=base(4,200)((/(i*1.2,i=1,200)/), 'xlftest again'))
end
