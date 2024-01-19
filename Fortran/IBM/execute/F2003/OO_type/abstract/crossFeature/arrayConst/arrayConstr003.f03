! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: array constructor
!*                                        Construct array of non-polymorphic type with poly abstract variable
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

   type :: b1
      integer :: id
   contains
      procedure, nopass :: print => printb1
   end type

   type, extends(b1), abstract :: b2
   contains
      procedure, nopass :: print => printb2
   end type

   type, extends(b2) :: b3
      contains
      procedure, nopass :: print => printb3
   end type

   interface
      integer function printif()
      end function
   end interface

contains

   integer function printb1()
      printb1 = 1
   end function

   integer function printb2()
      printb2 = 2
   end function

   integer function printb3()
      printb3 = 3
   end function

end module

program arrayConstr003
   use m

   class(b1), dimension(:), allocatable :: b11
   class(b1), dimension(:), pointer     :: b12

   class(b2), allocatable :: c1
   class(b1), pointer :: c2

   allocate (c1, source = b3(1) )
   allocate (c2, source = b3(3) )

   allocate(b11(2), source = (/ c1, c1 /) )
   allocate(b12(3), source = (/ b11((/2,1/)), c2 /) )

   if ( (b11(1)%id .ne. 1) .or. (b11(2)%id .ne. 1) ) error stop 1_4
   if ( (b12(1)%id .ne. 1) .or. (b12(2)%id .ne. 1) .or. (b12(3)%id .ne. 3))  error stop 2_4

   if (( b11(1)%print() .ne. 3 ) .or. (b11(2)%print() .ne. 3) )error stop 3_4
   if ( ( b12(1)%print() .ne. 3 ) .or. ( b12(2)%print() .ne. 3 ) .or.( b12(3)%print() .ne. 3 ) ) error stop 4_4

end program