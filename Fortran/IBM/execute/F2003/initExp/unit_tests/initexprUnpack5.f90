!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : UNPACK intrinsic
!*
!* DESCRIPTION                : character type
!* ===================================================================

implicit none

integer :: i, j

logical, parameter :: T=.true., F=.false.
logical, parameter :: msk4(3,3)=reshape((/F,T,F,T,F,F,T,F,T/), (/3,3/))

character, parameter :: v4(4)=(/'a','b','c',':'/)
character, parameter :: fld4(3,3)=reshape((/'Z','Y',';','?','O','D','P',' ','Q'/),(/3,3/))

character, dimension(3,3) :: res1=unpack(v4, msk4, fld4), &
  & res2=unpack(v4, msk4, field='s')

if (.not. all(res1 .eq. unpack(v4, msk4, fld4))) stop 1
if (.not. all(res2 .eq. unpack(v4, msk4, field='s'))) stop 2

end
