! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of type real*8
!*                               implicit real*8
!*                               FROM is component of a DT
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      implicit  real*8 (r)

      type base
          real*8, allocatable :: r2(:)
      end type

      allocatable :: r1(:)
      allocatable :: r2(:)

      type(base), pointer :: t

      allocate(r1(-10:-1), source=(/( real(i,8) ,i=1,10)/))
      allocate(r2(5), source = r1(-9::2))
      allocate(t, source = base(r1(-8::1)))

      call move_alloc(t%r2,r2)

      if ( allocated( t%r2) ) error stop 11
      if ( .not. allocated(r2) ) error stop 13

      if ( size(r2) /= 8 ) error stop 21

      write (*, '(4f12.8)') r2

    end
