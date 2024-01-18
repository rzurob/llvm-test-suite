!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : TRANSPOSE intrinsic
!*
!* DESCRIPTION                : character type
!* ===================================================================

implicit none

character(7), parameter, dimension(2,2) :: C=reshape( &
 & (/'IBM STG', 'IBM SWG', 'IBM IGS', 'IBM TOR'/), &
 & (/2,2/))

character(7) :: res3(2,2)=transpose(C)

if (.not. all(res3 .eq. transpose(C))) stop 3

end
