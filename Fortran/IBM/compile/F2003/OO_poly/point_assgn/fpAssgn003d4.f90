! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/06/2005
!*
!*  DESCRIPTION                : data pointer assignment (array section does not
!                               have POINTER attribute)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn003d4
    class (*), pointer :: x(:)
    integer*4, target :: i(10)
    x => i

    print *, associated (x(3))      !<-- this is invalid
    print *, associated (x (::2))   !<-- this is invalid
end
