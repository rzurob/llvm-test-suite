!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : CEILING intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================
program main
  integer(1) :: ii1=ceiling(9.51139_4, 1)
  integer(1) :: jj1=ceiling(9.51139_8, 1)
  integer(1) :: kk1=ceiling(9.51139_16, 1)

  integer(2) :: ii2=ceiling(9.51139_4, 2)
  integer(2) :: jj2=ceiling(9.51139_8, 2)
  integer(2) :: kk2=ceiling(9.51139_16, 2)

  integer(4) :: ii4=ceiling(9.51139_4, 4)
  integer(4) :: jj4=ceiling(9.51139_8, 4)
  integer(4) :: kk4=ceiling(9.51139_16, 4)

  integer(8) :: ii8=ceiling(9.51139_4, 8)
  integer(8) :: jj8=ceiling(9.51139_8, 8)
  integer(8) :: kk8=ceiling(9.51139_16, 8)
!
  if (.not. all((/ii1, jj1, kk1/) .eq. &
    & (/ceiling(9.51139_4, 1), ceiling(9.51139_8, 1), &
    &   ceiling(9.51139_16, 1)/))) error stop 1

  if (.not. all((/ii2, jj2, kk2/) .eq. &
    & (/ceiling(9.51139_4, 2), ceiling(9.51139_8, 2), &
    &   ceiling(9.51139_16, 2)/))) error stop 2

  if (.not. all((/ii4, jj4, kk4/) .eq. &
    & (/ceiling(9.51139_4, 4), ceiling(9.51139_8, 4), &
    &   ceiling(9.51139_16, 4)/))) error stop 3

  if (.not. all((/ii8, jj8, kk8/) .eq. &
    & (/ceiling(9.51139_4, 8), ceiling(9.51139_8, 8), &
    &   ceiling(9.51139_16, 8)/))) error stop 3

end program
