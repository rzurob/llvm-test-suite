!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - with deferred character allocatable array component
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
      character(:), allocatable ::c(:)
   end type

end module

type(base) function foo ( a )
   use m, only: base
   type(base), value :: a

   print *, 'foo:'
   print *, len(a%c), a%c

   deallocate ( a%c )
   allocate (a%c(3), source = (/ 'xxx', 'xxx', 'xxx' /) )

   print *, len(a%c), a%c

   foo = a

   print *, 'end:'

end function

program valueDeferredCharAllocatableComponent002
   use m

   type(base) :: b1, b2
   allocatable :: b2

   interface
      type(base) function foo ( a )
         import base
         type(base), value :: a
      end function
   end interface

   b1 = foo ( base((/''/)) )
   print *, 'b1:', len(b1%c), size(b1%c), b1%c

   allocate ( b2, source = foo ( b1 ) )
   print *, 'b2:', len(b2%c), size(b2%c), b2%c

   b2 = base( (/ 'abcdef', 'ghijkl', 'mnopqr' /) )

   b1 = foo ( b2 )
   print *, 'b1:', len(b1%c), size(b1%c), b1%c

   b2 = foo ( b2 )
   print *, 'b2:', len(b2%c), size(b2%c), b2%c

end program
