! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/mv_Alloc/typCompatible/deferchar4.f
! opt variations: -qnok -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
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

  type base(k1,n1)    ! (4,20)
      integer, kind :: k1
      integer, len  :: n1
     character(:, kind=1), allocatable :: ch2(:)
  end type

  type (base(4,20)) b1

  contains
      subroutine sub(arg)
          character(len =:), intent(inout), allocatable :: arg(:)
          call move_alloc(ch1, arg)
      end subroutine
end module

  use m

  allocate (ch1(4), source = (/ '1234567', 'abc efg', ' opqrst', 'uvw7890' /) )

  call sub(b1%ch2)

  if ( allocated(ch1) ) error stop 21
  if ( .not. allocated(b1%ch2)) error stop 23

  do i = 1, 4
     print *, b1%ch2(i)
  end do

  end
