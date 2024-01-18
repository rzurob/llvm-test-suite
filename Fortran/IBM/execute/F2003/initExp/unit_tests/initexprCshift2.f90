!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : CSHIFT intrinsic
!*
!* DESCRIPTION                : with DIM and SHIFT arg
!* ===================================================================

implicit none

character, dimension(3,3), parameter :: A=reshape((/'A','B','C','D', &
 & 'E','F','G','H','I'/), (/3,3/))
integer, dimension(2,2) :: i1=cshift(reshape((/1,2,3,4/),(/2,2/)), shift=1, dim=1)
integer, dimension(2,2) :: i2=cshift(reshape((/1,2,3,4/),(/2,2/)), shift=-3, dim=2)

character, dimension(3,3) :: res1=cshift(A, shift=(/-1,1,0/), dim=1), &
                             res2=cshift(A, shift=(/-1,1,0/), dim=2)

if (.not. all(i1 .eq. cshift(reshape((/1,2,3,4/),(/2,2/)), shift=1, dim=1))) &
 & stop 1

if (.not. all(i2 .eq. cshift(reshape((/1,2,3,4/),(/2,2/)), shift=-3, dim=2))) &
 & stop 2

if (.not. all(res1 .eq. cshift(A, shift=(/-1,1,0/), dim=1))) stop 3
if (.not. all(res2 .eq. cshift(A, shift=(/-1,1,0/), dim=2))) stop 4

end
