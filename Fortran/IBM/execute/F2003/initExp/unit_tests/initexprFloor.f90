!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : FLOOR intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

program main
  implicit none

  integer :: ii=floor(9.51139)
  integer :: jj=floor(9.51139, 4)
  integer :: kk=floor(kind=4, a=9.51139)
  integer(8) :: i8=floor(1349723384749.18734_8, kind=8)

  if (ii .ne. floor(9.51139)) stop 1
  if (jj .ne. floor(9.51139, 4)) stop 2
  if (kk .ne. floor(kind=4, a=9.51139)) stop 3

  if (i8 .ne. floor(1349723384749.18734_8, kind=8)) stop 4

end program
