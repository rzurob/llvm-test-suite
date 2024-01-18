! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: array constructor
!*                                        Construct array of unlimited polymorphic type with poly abstract variable
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

   type, abstract :: b1
      integer :: id
   contains
      procedure(printif), nopass, deferred :: print
   end type

   type, extends(b1) :: b2
   contains
      procedure, nopass :: print => printb2
   end type

   interface
      integer function printif()
      end function
   end interface

contains

   integer function printb2()
      printb2 = 2
   end function

end module

program arrayConstr004
   use m

   class(*), dimension(:), allocatable :: b11

   class(b1), allocatable :: c1
   allocate (c1, source = b2(1) )

   allocate(b11(2), source = (/ c1, c1 /) )

   select type (b11)
      class is (b1)
         if ( (b11(1)%id .ne. 1) .or. (b11(2)%id .ne. 1) ) error stop 1_4
         if ( (b11(1)%print() .ne. 2 ) .or. (b11(2)%print() .ne. 2) ) error stop 2_4
      class default
         error stop 3_4
   end select

end program