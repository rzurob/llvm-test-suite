!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : CEILING intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================
program main
  integer :: ii=ceiling(9.51139_4)
  integer :: jj=ceiling(9.51139_8)
  integer :: kk=ceiling(a=9.51139_16)

  if (ii /= ceiling(9.51139_4)) error stop 1
  if (jj /= ceiling(9.51139_8)) error stop 2
  if (kk /= ceiling(a=9.51139_16)) error stop 3
end program
