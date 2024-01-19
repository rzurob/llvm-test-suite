! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Secition 9.9.3 INQUIRE by output list
!*                               - inquire iolength of scalar polymorphic items
!*                                 when output items are structure/array constructor
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type :: base
      integer(8)   :: x
      real(4)      :: y
      character(3) :: z
   end type

   type, extends(base) :: child
      integer(2) :: a
      real(8)    :: b
      complex(8) :: c
   end type

   interface
      integer function getIOlengthArray (item)
         class(*) :: item(:)
      end function
   end interface

   interface
      integer function getIOlength (item)
         class(*) :: item
      end function
   end interface
end module

program constructor001
   use m1

   character(200) :: msg1 = ''
   integer :: stat1
   integer :: length1

   if ( getIOlength ( base(x=1, y=2.2, z='abc') ) /= 16 )            error stop 1_4

   if ( getIOlength ( child(1,2.3,'abc',4,5,(6.0,7.0)) ) /= 48 )     error stop 2_4

   if ( getIOlengthArray ( (/ base(x=1, y=2.2, z='abc'), base(x=3, y=4.5, z='abc') /) ) /= 32  )             error stop 3_4

   if ( getIOlengthArray ( (/ ( child(1,2.3,'abc',4,5,(6.0,7.0)), i=10,1,-2 ) /) )      /= 240 )             error stop 4_4

   if ( getIOlengthArray ( (/ ( child(1,2.3,'abc',4,5,(6.0,7.0)), i=10,1 ) /) )         /= 0 )               error stop 5_4


end program

integer function getIOlength (item)
use m1, only: base, child
   class(*) :: item
   integer :: length1 = -999999
   select type ( a => item )
      type is (base)
         inquire ( iolength = length1 ) a
      type is (child)
         inquire ( iolength = length1 ) a
      type is (integer)
         inquire ( iolength = length1 ) a
      class default
         error stop 6_4
   end select

   getIOlength = length1

end function

integer function getIOlengthArray (item)
use m1, only: base, child
   class(*) :: item(:)
   integer :: length1 = -999999
   select type ( a => item )
      type is (base)
         inquire ( iolength = length1 ) a
      type is (child)
         inquire ( iolength = length1 ) a
      type is (integer)
         inquire ( iolength = length1 ) a
      class default
         error stop 7_4
   end select

   getIOlengthArray = length1

end function