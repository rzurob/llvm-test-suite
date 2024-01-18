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
!*  DATE                       : 02/14/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.8: derived-type-spec)
!                               Case: Use of parameterized derived-type with
!                               default values as interface-name in procedure
!                               declaration stmt and dummy procedure.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k, n)
        integer, kind :: k = 4
        integer, len :: n = 50

        real(k) :: data(n) = sqrt(-1.0)
    end type
end module

module m1
use m
    type container
        class(base), pointer :: data => null()
    end type

    contains

    type(container) function genContainer (p1, r1)
        procedure(type(base)) :: p1
        real(4), intent(in) :: r1(50)

        allocate (genContainer%data, source=p1(r1))
    end function
end module

program dtparamDefVal010
use m1
    procedure(type(base)) :: genBase
    real(4) r1(100)

    type (container) co1

    logical(4), external :: precision_r4

    r1 = log((/(i*1.3e0, i=1, 100)/))

    co1 = genContainer(genBase, r1)

    if (.not. associated(co1%data)) error stop 1_4

    do i = 1, 50
        if (.not. precision_r4 (co1%data%data(i), 1.0+log(i*1.3e0))) &
                error stop 2_4
    end do
end

type (base) function genBase (r1)
use m
    real(4), intent(in) :: r1(50)

    genBase%data = r1 + 1.0
end function
