!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : MAXLOC intrinsic
!*
!* DESCRIPTION                : real type
!* ===================================================================

implicit none

real, parameter :: a(4,4)=reshape((/4.0,2.0,9.0,-7.0,9.0,1.0,4.0,5.0,8.0,-1.0,-1.0,7.0, &
                                 & -8.0,5.0,9.0,-3.0/),(/4,4/))

real, dimension(2) :: res1=maxloc(a)
real :: res2(4)=maxloc(a, dim=1, mask=a .lt. 7)

real, parameter :: b(-2:2)=(/1,2,3,4,5/)
real :: res3(1)=maxloc(b)

if (.not. all(res1 .eq. maxloc(a))) stop 1
if (.not. all(res2 .eq. maxloc(a, dim=1, mask=a .lt. 7.0))) stop 2
if (.not. all(res3 .eq. maxloc(b))) stop 3

end
