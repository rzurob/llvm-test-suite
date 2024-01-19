!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : ADJUSTL on initialization expression
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

character(5), parameter :: c1='abc  '
character(5) :: c2=adjustl(c1)
end
