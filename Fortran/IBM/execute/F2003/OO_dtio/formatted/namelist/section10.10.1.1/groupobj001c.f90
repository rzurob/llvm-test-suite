!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Section 10.10.1.1 Namelist group object names
!*                                        Input data being object components
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
      integer :: i
   end type

   type container
      type(base) :: b
   end type

end module

program groupobj001c
use m

   type(container) :: b1
   character(200) :: msg = ''
   integer :: stat
   namelist /n1/ b1

   b1 = container ( base (-999) )
   open (1, file='groupobj001c.1', form='formatted', access='sequential' )
   read (1, n1, iostat=stat,iomsg = msg)

   if ( ( stat /= 0 ) .or. ( msg /= '') ) error stop 1_4
   if ( b1%b%i /= 10 ) error stop 2_4

end

