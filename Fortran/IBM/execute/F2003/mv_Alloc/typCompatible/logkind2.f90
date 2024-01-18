! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of logical*2
!*                               use implicit to specify logical*2
!*                               -qintsize = 2
!*                               default logical type is kind 4
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

          implicit logical*2 ( l )

          allocatable :: l2(:,:,:)
          logical , allocatable :: l1(:,:,:)

          allocate(l1(3,3,3))

          l1 = .false.
          l1(:, :, 3) = .true.

          call move_alloc(l1, l2)

          if ( allocated(l1)) stop 11
          if ( .not. allocated(l2)) stop 13

          if ( size(l2,2) /= 3 ) stop 21
          if ( l2(2,1,3) .neqv. .true. ) stop 23
          if ( l2(1,3,2) .neqv. .false. ) stop 25

          end
