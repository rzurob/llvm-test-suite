!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : SPREAD intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

real(4), dimension(2,2) :: r4=spread((/1.0,2.0/), dim=1, ncopies=2)
real(8), dimension(2,2) :: r8=spread((/1.0,2.0/), dim=1, ncopies=2)
real(16), dimension(2,2) :: r16=spread((/1.0,2.0/), dim=1, ncopies=2)

complex(4), dimension(2,2) :: c4=spread((/(1.0,2.0),(1.0,2.0)/), dim=1, ncopies=2)
complex(8), dimension(2,2) :: c8=spread((/(1.0,2.0),(1.0,2.0)/), dim=1, ncopies=2)
complex(16), dimension(2,2) :: c16=spread((/(1.0,2.0),(1.0,2.0)/), dim=1, ncopies=2)

integer(1), dimension(2,2) :: i1=spread((/1,2/), dim=1, ncopies=2)
integer(2), dimension(2,2) :: i2=spread((/1,2/), dim=1, ncopies=2)
integer(4), dimension(2,2) :: i4=spread((/1,2/), dim=1, ncopies=2)
integer(8), dimension(2,2) :: i8=spread((/1,2/), dim=1, ncopies=2)

logical(1), dimension(2,2) :: l1=spread((/.true.,.false./), dim=1, ncopies=2)
logical(2), dimension(2,2) :: l2=spread((/.true.,.false./), dim=1, ncopies=2)
logical(4), dimension(2,2) :: l4=spread((/.true.,.false./), dim=1, ncopies=2)
logical(8), dimension(2,2) :: l8=spread((/.true.,.false./), dim=1, ncopies=2)

character, dimension(2,2) :: c=spread((/'a','z'/),dim=2,ncopies=2)

end
