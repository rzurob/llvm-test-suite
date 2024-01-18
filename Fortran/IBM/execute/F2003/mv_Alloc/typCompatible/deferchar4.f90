! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : deferchar4.f
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
!*  DESCRIPTION                : FROM/TO are of allocatable array with
!*                               deferred len of character
!*                               TO is dummy arg of moudle procedure 
!*                               see 321721
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

  character(len =:), allocatable :: ch1(:)

  type base
     character(:, kind=1), allocatable :: ch2(:)
  end type

  type (base) b1

  contains
      subroutine sub(arg)
          character(len =:), intent(inout), allocatable :: arg(:) 
          call move_alloc(ch1, arg)
      end subroutine
end module

  use m

  allocate (ch1(4), source = (/ '1234567', 'abc efg', ' opqrst', 'uvw7890' /) )

  call sub(b1%ch2)

  if ( allocated(ch1) ) stop 21
  if ( .not. allocated(b1%ch2)) stop 23

  do i = 1, 4
     print *, b1%ch2(i)
  end do

  end
