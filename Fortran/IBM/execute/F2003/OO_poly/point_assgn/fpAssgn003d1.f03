! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2005
!*
!*  DESCRIPTION                : data pointer assignment (for associated()
!                               intrinsic if TARGET appears, it must be of an
!                               object that is allowed as if in a pointer
!                               assignment statement; test using mis-match of
!                               the rank)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn003d1
    integer*4, pointer :: x(:), x1
    integer*4, target :: i, i1(10)

    class (*), pointer :: x3, x4(:)

    x => i1
    x1 => i

    x4 => i1
    x3 => x4(1)

    print *, associated (x, i)      !<-- illegal
    print *, associated (x1, i1)    !<-- illegal
    print *, associated (x4, x3)    !<-- illegal
end
