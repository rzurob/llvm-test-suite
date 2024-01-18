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
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with (non-)polymorphic allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable) with optional attribute
!*                                 - dummy arg: non-polymorphic with value attribute
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
      integer, allocatable :: i
   end type

   type, extends(inner) :: cinner
      integer, allocatable :: j
   end type

   type base
      type(inner), allocatable :: in1
   end type

   type, extends(base) :: child
      class(inner), allocatable :: in2
   end type

   contains

   subroutine foo ( a, b )
      type(base), value, optional :: a
      type(base), value, optional :: b

      print *, 'foo:'
      if ( present(a) ) then
         print *, a%in1%i
         a%in1%i = -999
         print *, a%in1%i
      end if

      if ( present(b) ) then
         print *, b%in1%i
         b%in1%i = -999
         print *, b%in1%i
      end if

   end subroutine

   subroutine bar ( a, b )
      type(child), value, optional :: a
      type(child), value, optional :: b

      print *, 'bar:'
      if ( present(a) ) then
         print *, a%in1%i
         a%in1%i = -999
         print *, a%in1%i

         select type ( g => a%in2 )
            type is ( inner )
               print *, g%i
               g%i = -999
               print *, g%i
            type is ( cinner )
               print *, g%i, g%j
               g%i = -999
               g%j = -999
               print *, g%i, g%j
         end select
      end if

      if ( present(b) ) then
         print *, b%in1%i
         b%in1%i = -999
         print *, b%in1%i
         
         select type ( g => b%in2 )
            type is ( inner )
               print *, g%i
               g%i = -999
               print *, g%i
            type is ( cinner )
               print *, g%i, g%j
               g%i = -999
               g%j = -999
               print *, g%i, g%j
         end select
      end if

   end subroutine

end module

program valueOptionalDummyArg002
   use m

   type(base), pointer :: b1
   class(base), allocatable :: b2
   type(child), allocatable :: c1
   class(child), pointer :: c2

   allocate ( b1, source = base(inner(100)) )
   allocate ( b2, source = child( inner(200), inner(300) ) )

   call foo()
   call foo(b=b1)
   print *, b1%in1%i
   call foo(b2)
   print *, b2%in1%i
   call foo(b=b2,a=b1)
   print *, b1%in1%i, b2%in1%i
   
   allocate ( c1, source = child ( inner(400), cinner(500, 600) ) )
   allocate ( c2, source = child ( inner(4000), cinner(5000, 6000) ) )

   call bar()
   call bar(b=c1)
   print *, c1%in1%i
   select type ( g => c1%in2 )
      type is ( inner )
         print *, g%i
      type is ( cinner )
         print *, g%i, g%j
   end select
   
   call bar(c2)
   print *, c2%in1%i
   select type ( g => c2%in2 )
      type is ( inner )
         print *, g%i
      type is ( cinner )
         print *, g%i, g%j
   end select
   
   call bar(b=c2,a=c1)
   print *, c1%in1%i
   select type ( g => c1%in2 )
      type is ( inner )
         print *, g%i
      type is ( cinner )
         print *, g%i, g%j
   end select
   
   print *, c2%in1%i
   select type ( g => c2%in2 )
      type is ( inner )
         print *, g%i
      type is ( cinner )
         print *, g%i, g%j
   end select

end program
