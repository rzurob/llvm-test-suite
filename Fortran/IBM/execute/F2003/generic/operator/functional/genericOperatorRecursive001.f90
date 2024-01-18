!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : operator: function being recursive and assigning linked lists
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
      type(base), pointer :: next => null()
      contains
         procedure, pass :: badd
         generic :: operator(+) => badd
   end type

   contains

   recursive function badd ( a, b )
      class(base), intent(in) :: a
      type(base), intent(in)   :: b
      type(base)::badd

      badd%i = a%i + b%i

      if ( associated ( b%next ) .and. associated ( a%next ) ) then
         allocate ( badd%next, source = a%next + b%next )
      end if

   end function

end module


program genericOperatorRecursive001
   use m

   type(base), target :: b1
   type(base), target :: b2, b3

   type(base), pointer :: tmp => null()

   b1 = base( 100, null() )
   tmp => b1

   do i = 1, 9
      allocate ( tmp%next, source = base((i+1)*100, null() ) )
      tmp => tmp%next
   end do

   b2 = base( 1000, null() )
   tmp => b2

   do i = 1, 9
      allocate ( tmp%next, source = base((i+1)*1000, null() ) )
      tmp => tmp%next
   end do

   print *, 'Linked List 1'
   b3 = b1 + b2
   tmp => b3
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do

   print *, 'Linked List 2'
   b1 = b1 + b2 + b3
   tmp => b1
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do
   
   print *, 'Linked List 3'
   b2 = base(10, null() ) + base(20, null() )
   tmp => b2
   do while ( associated ( tmp ) )
      print *, tmp%i
      tmp => tmp%next
   end do


end program
