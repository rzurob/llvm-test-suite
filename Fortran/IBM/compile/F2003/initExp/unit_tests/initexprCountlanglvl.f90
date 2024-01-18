!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : COUNT intrinsic
!*
!* DESCRIPTION                : langlvl message
!* ===================================================================

implicit none
integer(1) :: e1T=count((/.true._1/))
integer(2) :: e2T=count((/.true._2/))
integer(4) :: e4T=count((/.true._4/))
integer(8) :: e8T=count((/.true._8/))
end
