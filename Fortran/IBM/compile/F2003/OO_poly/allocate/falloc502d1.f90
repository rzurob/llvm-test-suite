! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 01/27/2005
!*
!*  DESCRIPTION                : allocate (allocatable array element does not
!                               have allocatable attribute)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc502d1
    class (*), allocatable :: x (:,:)

    allocate (x (2, 2), source=(1.0d0, 1.0d0))

    print *, allocated (x(1,1))     !<-- array element has no allocatable attr.
    print *, allocated (x(:,1:2))   !<-- array section has no allocatable attr.
end
