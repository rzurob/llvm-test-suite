!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : PACK intrinsic
!*
!* DESCRIPTION                : character type
!* ===================================================================

implicit none
integer :: i

character, parameter :: i0(3,2)=reshape((/'a','b','c','d','e','g'/),(/3,2/))
character :: c1(6)=pack(i0, mask=i0 .ne. 'f')
character :: c2(10)=pack(i0, mask=i0 .eq. 'c', vector=(/('z',i=1,10)/))

if (.not. all(c1 .eq. pack(i0, mask=i0 .ne. 'f'))) error stop 1
if (.not. all(c2 .eq. pack(i0, mask=i0 .eq. 'c', vector=(/('z',i=1,10)/)))) error stop 2
end