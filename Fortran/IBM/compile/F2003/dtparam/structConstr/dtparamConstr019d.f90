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
!*  DATE                       : 03/02/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constr)
!                               Case: Generic name and structure constructor
!                               collision: incorrect reference to structure
!                               constructor.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k
        integer, len :: n

        real(k) :: data(n)
    end type

    interface base
        module procedure genBase4_1
        module procedure genBase4_2
    end interface

    contains

    function genBase4_1 (val)
        real(4), intent(in) :: val(:)

        type(base(4, size(val))) genBase4_1

        genBase4_1%data = val
    end function

    function genBase4_2 (i, val)
        integer, intent(in) :: i
        real(4), intent(in) :: val(i)

        type(base(4,i)) genBase4_2

        genBase4_2%data = val
    end function
end module

program dtparamConstr019d
use m
    type (base(4, 25)) b1

    type (base(4, :)), allocatable :: b2

    !! illegal use of structure constructors
    b1 = base(4,25)(val=(/(i*1.0,i=1,25)/))

    allocate (base(4,33) :: b2)

    b2 = base(4,33)(33, (/(i+1.2e0, i=1,33)/))
end
