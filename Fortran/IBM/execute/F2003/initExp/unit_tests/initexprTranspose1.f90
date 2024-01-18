!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : TRANSPOSE intrinsic
!*
!* DESCRIPTION                : integer type
!* ===================================================================

implicit none

integer(1), parameter, dimension(3,4) :: a=reshape( &
 & (/0_1,2_1,7_1,-5_1,4_1,5_1,8_1,-1_1,6_1,-7_1,1_1,6_1/), &
 & (/3,4/))

integer(2), parameter, dimension(11,2) :: B=reshape( &
 & (/31_2, 71_2, 43_2, 22_2, 16_2, 02_2, 47_2, 66_2, 75_2, 17_2, &
 &   88_2, 65_2, 30_2, 97_2, 76_2, 72_2, 06_2, 21_2, 10_2, 41_2, &
 &   24_2, 30_2/), &
 & (/11,2/))

integer(4), parameter, dimension(2,2) :: C=reshape( &
 & (/225044416, 29175423, 1521731315, 1107175217/), &
 & (/2,2/))

integer(8), parameter, dimension(7,1) :: D=reshape( &
 & (/664313279, 394649677, 614318288, 1318726915, 451536740, 969358293, 1261183769/), &
 & (/7,1/))


integer(1), dimension(4,3) :: res1=transpose(a)
integer(2), dimension(2,11) :: res2=transpose(B)
integer(4), dimension(2,2) :: res3=transpose(C)
integer(8), dimension(1,7) :: res4=transpose(D)

if (.not. all(res1 .eq. transpose(a))) stop 1
if (.not. all(res2 .eq. transpose(B))) stop 2
if (.not. all(res3 .eq. transpose(C))) stop 3
if (.not. all(res4 .eq. transpose(D))) stop 4

end
