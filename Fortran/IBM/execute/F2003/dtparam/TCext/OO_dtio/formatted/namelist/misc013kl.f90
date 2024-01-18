!*  ===================================================================
!*
!*  TEST CASE NAME             : misc013kl
!*
!*  DATE                       : 2007-07-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
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
   type base (kb,lb)
      integer, kind :: kb
      integer, len :: lb
      character(lb) :: c
      integer(kb) :: i
   end type
end module

program misc013kl
   use m

   type(base(4,3))              :: c1(3) = (/ base(4,3)('ABC',101), base(4,3)('DEF',102), base(4,3)('GHI',103) /) ! tcx: (4,3) ! tcx: (4,3) ! tcx: (4,3) ! tcx: (4,3)
   class(base(4,:)), pointer    :: c2(:) ! tcx: (4,:)

   allocate ( c2(3), source = c1 )
   print *,  c1%c
   print *,  c2%c
   print *,  c2(1)%c, c2(2)%c, c2(3)%c

end program





! Extensions to introduce derived type parameters:
! type: base - added parameters (kb,lb) to invoke with (4,3) / declare with (4,*) - 5 changes
