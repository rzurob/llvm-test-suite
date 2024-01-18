!*  ===================================================================
!*
!*  TEST CASE NAME             : misc006kl
!*
!*  DATE                       : 2007-07-06 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: implicit statement with allocate statement (defect 299134)
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
   type base (kb)
      integer, kind :: kb
      integer(kb) :: i = 12
   end type

   implicit type(base(4)) (z) ! tcx: (4)
   allocatable :: z1, z2(:)

end module

program misc006kl
   use m

   allocate(z1)
   allocate(z2(2))

   if ( z1%i /= 12 ) error stop 1_4
   if (( z2(1)%i /= 12 ) .or. ( z2(2)%i /= 12 ) ) error stop 2_4

end program


! Extensions to introduce derived type parameters:
! type: base - added parameters (kb) to invoke with (4) / declare with (4) - 1 changes
