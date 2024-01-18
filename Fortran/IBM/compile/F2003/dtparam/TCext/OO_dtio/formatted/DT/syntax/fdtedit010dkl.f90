!*  ===================================================================
!*
!*  TEST CASE NAME             : fdtedit010dkl
!*
!*  DATE                       : 2007-06-08 (original: 21/03/2005)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Test if compiler complains
!*                                        when there is no DTIO interface present
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
      integer(kb) :: i
   end type

end module

program fdtedit010dkl
use m

   class(base(4)), allocatable :: b1

   integer(4) :: stat
   character(150) :: msg

   open (1, file = 'fdtedit010dkl.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(4)(100) )

   write ( 1, fmt="(DT)" )   b1
   rewind 1
   read ( 1,  "(DT)" )       b1

end program
