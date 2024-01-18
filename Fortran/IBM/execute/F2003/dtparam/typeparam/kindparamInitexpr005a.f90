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
!*  DATE                       : 12/29/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.2: type parameters)
!                               Case: Kind type parameter used in the kind type
!                               param for components: procedure pointer
!                               components.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k = 4

        procedure(real(k)), pointer , nopass:: add => null()
        real(k) data
    end type
end module

program kindparamInitexpr005a
use m
    type(base) b1
    type(base(8)) b2

    logical(4) precision_r8, precision_r4
    real(4) addR4
    real(8) addR8

    external precision_r8, precision_r4, addR4, addR8

    b1%data = tan(.3e0)
    b2%data = dtan(.3d0)

    b1%add => addR4
    b2%add => addR8

    if (.not. precision_r4(b1%add(b1%data, 0.3e0), 0.60933626e0)) &
            error stop 1_4

    if (.not. precision_r8(b2%add(b2%data, 0.3d0), 0.609336249609623d0)) &
            error stop 2_4
end

real(4) function addR4 (i1, i2)
    real(4) i1, i2

    addR4 = i1 + i2
end function

real(8) function addR8 (i1, i2)
    real(8) i1, i2

    addR8 = i1 + i2
end function
