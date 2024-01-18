! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : dblprecision.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
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
 
      if ( .not. allocated(d1) ) stop 11
      if ( allocated(d2) ) stop 13

      if ( size(d1, 1) /= 2) stop 21 
      if ( size(d1, 2) /= 3) stop 23

      do j = 1, 3
          do i = 1, 2
              if ( .not. precision_R8(d1(i,j),dble(i+2*j-6)) ) stop 25
          end do
      end do

      end
