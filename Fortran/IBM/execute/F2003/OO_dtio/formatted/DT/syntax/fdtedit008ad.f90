!*  ===================================================================
!*
!*  DATE                       : 21/03/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.6.5 DT edit descriptor
!*                                        Test if compiler complains when
!*                                        list item being non-derived type (intrinsic type) (runtime format)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fdtedit008ad

   integer(4) :: stat
   character(150) :: msg
   character(4)   :: fmt = "(DT)"
   open (1, file = 'fdtedit008ad.1', form='formatted', access='sequential' )

   write ( 1, fmt )  'abc'

end program
