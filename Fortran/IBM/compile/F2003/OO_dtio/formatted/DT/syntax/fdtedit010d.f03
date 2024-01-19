!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
   type base
      integer(4) :: i
   end type

end module

program fdtedit010d
use m

   class(base), allocatable :: b1

   integer(4) :: stat
   character(150) :: msg

   open (1, file = 'fdtedit010d.1', form='formatted', access='sequential' )

   allocate ( b1, source = base(100) )

   write ( 1, fmt="(DT)" )   b1
   rewind 1
   read ( 1,  "(DT)" )       b1

end program
