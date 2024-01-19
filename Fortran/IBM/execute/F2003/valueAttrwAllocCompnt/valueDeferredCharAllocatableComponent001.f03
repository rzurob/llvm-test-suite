!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - with deferred character allocatable component
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

   type inner
      character(:), allocatable :: cc
   end type

   type base
      character(:), allocatable ::c
      type(inner), allocatable :: in
   end type

end module

type(base) function foo ( a )
   use m, only: base
   type(base), value :: a

   print *, 'foo:'
   print *, len(a%c), a%c
   print *, len(a%in%cc), a%in%cc

   deallocate ( a%c, a%in%cc )
   allocate (a%c, source = 'zzzzz')
   allocate (a%in%cc, source = '' )

   print *, len(a%c), a%c
   print *, len(a%in%cc), a%in%cc

   foo = a

   print *, 'end:'

end function

program valueDeferredCharAllocatableComponent001
   use m

   type(base) :: b1, b2
   allocatable :: b2

   interface
      type(base) function foo ( a )
         import base
         type(base), value :: a
      end function
   end interface

   b1 = foo ( base('', inner('')) )

   print *, b1%c
   print *, b1%in%cc

   allocate ( b2, source = foo ( b1 ) )

   print *, b1%c
   print *, b1%in%cc

   print *, b2%c
   print *, b2%in%cc

   b2 = foo ( base( 'ibm', inner('ftn2003')) )

   print *, b2%c
   print *, b2%in%cc

   b2 = base( 'ibm', inner('ftn2003'))

   b1 = foo ( b2 )

   print *, b2%c
   print *, b2%in%cc

   print *, b1%c
   print *, b1%in%cc

end program
