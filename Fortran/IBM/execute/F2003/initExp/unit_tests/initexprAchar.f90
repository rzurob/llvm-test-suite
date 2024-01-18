!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ACHAR on initialization expression
!*
!* DESCRIPTION                : integer type
!* ===================================================================

character(1), parameter :: c=achar(66)
character(1) :: c1=achar(43_1)
character(1) :: c2=achar(68_2)
character(1) :: c3=achar(70_4)
character(1) :: c4=achar(91_8)

if (c1 /= achar(43_1)) error stop 1
if (c2 /= achar(68_2)) error stop 2
if (c3 /= achar(70_4)) error stop 3
if (c4 /= achar(91_8)) error stop 4
if (c  /= achar(66)) error stop 5
end
