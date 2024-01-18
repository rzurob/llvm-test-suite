!* ===================================================================
!* XL FORTRAN TEST CASE                          IBM INTERNAL USE ONLY
!* ===================================================================
!* TEST CASE TITLE            : Initialization expression
!*
!* PROGRAMMER                 : Kelvin Li
!* DATE                       : March 31, 2006
!* ORIGIN                     : XL Compiler Development, Toronto Lab
!*
!* PRIMARY FUNCTIONS TESTED   : MINVAL intrinsic
!*
!* DESCRIPTION                : character type
!* ===================================================================

implicit none

character(2), parameter, dimension(3,3) :: c=reshape((/'aa','tz',' c', &
                                      &  'ee','ed','fh', &
                                      &  'ai','g ','h '/), (/3,3/))
character(2) :: res1(3)=minval(c, dim=1)
character(2) :: res2(3)

res2 = minval(c, dim=1)
if (.not. all(res1 .eq. res2)) stop 1

end
