!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/19/2006
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Type-parameter values for components can
!                               be initialization expression: from subobject of
!                               named constants. (C445)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamCompDecl001a
use m
    type (base), allocatable :: b1(:)

    logical(4), external :: precision_r8, precision_r4

    allocate (b1(100))

    if (b1(10)%id /= -1) error stop 1_4

    if (.not. precision_r8(b1(100)%a1%data, 1.1d0))  error stop 2_4

    allocate (b1(2)%a2(10), source=a4_const)

    if (.not. precision_r4(b1(2)%a2(8)%data, 1.0e0)) error stop 3_4
end
