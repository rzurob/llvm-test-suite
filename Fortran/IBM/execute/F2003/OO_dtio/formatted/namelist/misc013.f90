!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: print structure components (defect 301054)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
   type base
      character(3) :: c
      integer(4) :: i
   end type
end module

program misc013
   use m

   type(base)              :: c1(3) = (/ base('ABC',101), base('DEF',102), base('GHI',103) /)
   class(base), pointer    :: c2(:)

   allocate ( c2(3), source = c1 )
   print *,  c1%c
   print *,  c2%c
   print *,  c2(1)%c, c2(2)%c, c2(3)%c

end program



