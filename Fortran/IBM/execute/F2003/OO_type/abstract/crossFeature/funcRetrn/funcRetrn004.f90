! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Function subprogram (Section 12.5.2.1), class(abstract type)
!*                                        returns polymorphic abstract base type in subfunction
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

   type, abstract :: base
      integer :: id
   end type

   type, extends(base) :: child
   end type


contains

   class(base) function foo(a)
      pointer :: foo
      class(base), intent(in) :: a
      allocate(foo, source=innerfoo(a) )
   contains
      function innerfoo(a) result(boo)
         class(base), pointer :: boo
         class(base), intent(in) :: a
         allocate(boo, source=a)
      end function
   end function

end module

program funcRetrn004
   use m

   class(base), allocatable :: c
   class(base), allocatable :: b1
   allocate (b1, source = child(4))
   allocate ( c,source=foo(b1) )

   if ( c%id .ne. 4) error stop 1_4

end program

