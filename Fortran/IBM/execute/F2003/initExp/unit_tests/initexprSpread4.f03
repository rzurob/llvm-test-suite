!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : SPREAD intrinsic
!*
!* DESCRIPTION                : logical type
!* ===================================================================

implicit none
integer  i

logical, parameter :: T=.true., F=.false.

logical(1) :: resA(4,4)=spread((/T,F,T,F/), dim=1, ncopies=4)

logical(2), parameter :: a(6)=(/T, F, F, F, T, T/)
logical(2), dimension(6,6) :: res1=spread(a, dim=1, ncopies=6), &
        res2=spread(a, dim=2, ncopies=6)

logical(4), dimension(4,4) :: res4=spread((/F, T, T, F/), dim=2, ncopies=4)

logical(8), dimension(5,5), parameter :: c=reshape((/(T,i=1,25)/),(/5,5/))
logical(8), dimension(5,5,4) :: c1=spread(c, dim=3, ncopies=4)

if (.not. all(resA .eqv. spread((/T,F,T,F/), dim=1, ncopies=4))) error stop 1

if (.not. all(res1 .eqv. spread(a, dim=1, ncopies=6))) error stop 2
if (.not. all(res2 .eqv. spread(a, dim=2, ncopies=6))) error stop 3

if (.not. all(res4 .eqv. spread((/F, T, T, F/), dim=2, ncopies=4))) error stop 4

if (.not. all(c1 .eqv. spread(c, dim=3, ncopies=4))) error stop 5

end