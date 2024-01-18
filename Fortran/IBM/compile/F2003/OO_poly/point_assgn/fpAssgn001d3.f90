! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 04/28/2005
!*
!*  DESCRIPTION                : data pointer assignment (rank mismatch for
!                               data pointer assignment where no
!                               bounds-remapping get involved)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn001d3
    class (*), pointer :: x(:,:)

    integer, target :: i(10)

    x => i      !<-- illegal; NOTE: error message may change from release to rel
end
