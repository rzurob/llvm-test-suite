! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of type dbl precision
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      double precision, allocatable :: d1(:,:), d2(:,:)
      integer i
      logical precision_R8

      allocate(d1(-1,-2))
      allocate(d2(2,3), source = reshape((/ ( dble(i), i= -3,2 ) /), &
                  (/2,3/) ))

      call move_alloc(d2, d1)

      if ( .not. allocated(d1) ) error stop 11
      if ( allocated(d2) ) error stop 13

      if ( size(d1, 1) /= 2) error stop 21
      if ( size(d1, 2) /= 3) error stop 23

      do j = 1, 3
          do i = 1, 2
              if ( .not. precision_R8(d1(i,j),dble(i+2*j-6)) ) error stop 25
          end do
      end do

      end