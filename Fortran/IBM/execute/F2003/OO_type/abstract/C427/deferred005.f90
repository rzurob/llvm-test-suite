! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: if the type definition contains or inherits
!*                                        a deferred binding, ABSTRACT shall appear. (C427)
!*                                        v)	Type defintion contains and inherits deferred bindings,
!*                                              ABSTRACT defined with different interface (but same interface properties)
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

   type, abstract :: b1
      integer :: id
   contains
      procedure(printif1), pass, deferred :: print
   end type

   interface
      integer function printif1(a)
         import b1
         class(b1), intent(in) :: a
      end function
   end interface

end module

module m2
   use :: m1, newb1 => b1

   type, extends(newb1), abstract :: b2
   contains
      procedure(printif2), pass, deferred :: print
   end type

   type, extends(b2) :: b3
   contains
      procedure, pass :: print => printb3
   end type

   interface
      integer function printif2(a)
         import b2
         class(b2), intent(in) :: a
      end function
   end interface

contains

   integer function printb3(a)
      class(b3), intent(in) :: a
      printb3=a%id
   end function

end module

program deferred005
   use m2

   class(newb1), pointer :: b11
   class(b2), allocatable :: b21

   allocate (b11, source = b3(2) )
   allocate (b21, source = b3(3) )

   if ( b11%print() .ne. 2 ) error stop 1_4
   if ( b21%print() .ne. 3 ) error stop 2_4

end program