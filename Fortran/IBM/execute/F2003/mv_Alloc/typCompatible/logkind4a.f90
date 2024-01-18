! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of logical
!*                               rank 7, for some dimensions, zero size
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

        logical, allocatable :: l1(:,:,:,:,:,:,:), l2(:,:,:,:,:,:,:)

        l2 = func()

        call move_alloc(l2, l1)

        if ( .not. allocated(l1) ) stop 21
        if ( allocated(l2) ) stop 23

        print *, shape(l1)

        contains
                logical function func()
                        allocatable :: func(:,:,:,:,:,:,:)
                        allocate(func(1:0, 3:-1, 0, 2:3, 4:3,2:0,-1:-1))
                end function
       end
