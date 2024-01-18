!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : CEILING intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

program main

  integer :: ii=ceiling(9.51139)
  integer :: jj=ceiling(9.51139, 4)
  integer :: kk=ceiling(kind=4, a=9.51139)

  integer(1) :: i1=ceiling(126.189, kind=1)
  integer(2) :: i2=ceiling(7.8_4, kind=2)
  integer(4) :: i4=ceiling(834.147101_4)
  integer(8) :: i8=ceiling(835837384021.99072_8)

  if (.not. all((/ii, jj, kk/) .eq. &
    & (/ceiling(9.51139),ceiling(9.51139, 4),ceiling(kind=4, a=9.51139)/))) then
    stop 1
  endif

  if (i1 /= ceiling(126.189, kind=1)) error stop 1
  if (i2 /= ceiling(7.8_4, kind=2)) error stop 2
  if (i4 /= ceiling(834.147101_4)) error stop 3
  if (i8 /= ceiling(835837384021.99072_8)) error stop 4

end program
