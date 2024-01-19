!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : FLOOR intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

program main
  implicit none

  integer :: kk=floor(kind=4, a=9.51139)
  integer(8) :: i8=floor(1349723384749.18734_8, kind=8)
end program
