! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM/TO are of type character(*)
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

  character(7), allocatable :: ch2(:)

  character(7), allocatable :: ch1(:)

  allocate (ch1(4), source = (/ '1234567', 'abc efg', ' opqrst', 'uvw7890' /) )

  call sub(ch1, ch2)

  if ( allocated(ch1) ) stop 12
  if (.not. allocated(ch2) ) stop 23

  do i = 1, 4
     print *, ch2(i)
  end do

  contains
      subroutine sub(arg1, arg2)
          character(*), intent(inout), allocatable :: arg1(:)
          character(*), intent(inout), allocatable :: arg2(:)
          call move_alloc(arg1, arg2)
      end subroutine
  end
