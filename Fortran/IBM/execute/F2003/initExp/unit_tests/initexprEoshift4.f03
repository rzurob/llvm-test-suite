!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : EOSHIFT intrinsic
!*
!* DESCRIPTION                : character type
!* ===================================================================

implicit none

character, parameter, dimension(3,3) :: c0=reshape((/'A','D','G','B','E','H','C','F','I'/),(/3,3/))
character, dimension(3,3) :: c1=eoshift(c0, shift=-1, boundary='*', dim=2), &
  & c2=eoshift(c0, shift=(/-1,1,0/), boundary=(/'*','/','?'/), dim=2)

character, dimension(3,3) :: &
  & res1=reshape((/'*','*','*','A','D','G','B','E','H'/),(/3,3/)), &
  & res2=reshape((/'*','E','G','A','F','H','B','/','I'/),(/3,3/))

if (.not. all(c1 .eq. res1)) error stop 1
if (.not. all(c2 .eq. res2)) error stop 2

end