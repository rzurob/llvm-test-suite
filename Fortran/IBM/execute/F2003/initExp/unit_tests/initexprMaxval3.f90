!* ===================================================================
!*
!* DATE                       : March 31, 2006
!*
!* PRIMARY FUNCTIONS TESTED   : MAXVAL intrinsic
!*
!* DESCRIPTION                : character type
!* ===================================================================

implicit none

character(2), parameter, dimension(3,3) :: c=reshape((/'aa','tz',' c', &
                                      &  'ee','ed','fh', &
                                      &  'ai','g ','h '/), (/3,3/))
character(2) :: res1(3)=maxval(c, dim=1)
character(2) :: res2(3)

res2 = maxval(c, dim=1)
if (.not. all(res1 .eq. res2)) error stop 1

end
