!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : MINLOC intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

implicit none

integer, parameter :: a(4,4)=reshape((/4,2,9,-7,9,1,4,5,8,-1,-1,7, &
                                     & -8,5,9,-3/),(/4,4/))

integer, dimension(2) :: res1=minloc(a)
integer :: res2(4)=minloc(a, dim=1, mask=a .lt. 7)

integer, parameter :: b(-2:2)=(/1,2,3,4,5/)
integer :: res3(1)=minloc(b)

if (.not. all(res1 .eq. minloc(a))) stop 1
if (.not. all(res2 .eq. minloc(a, dim=1, mask=a .lt. 7))) stop 2
if (.not. all(res3 .eq. minloc(b))) stop 3

end
