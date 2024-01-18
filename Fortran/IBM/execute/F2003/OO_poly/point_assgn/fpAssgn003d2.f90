!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2005
!*
!*  DESCRIPTION                : data pointer assignment (for associated()
!                               intrinsic TARGET must be of a type that is
!                               allowed in a data pointer assignment statement)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn003d2
    type A
        integer i
    end type

    type B
        type (A) a1
    end type

    integer, pointer :: i_ptr
    type(A), pointer :: a_ptr

    type (A), target :: aa
    type (B), target :: bb

    i_ptr => aa%i

    a_ptr => bb%a1

    print *, associated (i_ptr, aa), associated (a_ptr, bb) !<-- illegal call

end

