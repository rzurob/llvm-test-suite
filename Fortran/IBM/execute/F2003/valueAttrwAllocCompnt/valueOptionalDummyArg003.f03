!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing unlimited polymorphic allocatable components
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

   type base
      class(*), allocatable :: in1
      class(*), pointer     :: in2
   end type

   integer, target :: nil = -999

   contains

   subroutine foo ( a, b )
      type(base), value, optional :: a
      type(base), value, optional :: b

      print *, 'foo:'
      if ( present(a) ) then
         select type ( g => a%in1 )
            type is ( integer )
               print *, g
            type is ( inner )
               print *, g%i
         end select
         select type ( g => a%in2 )
            type is ( integer )
               print *, g
            type is ( inner )
               print *, g%i
         end select

         a = base(-9, nil)

      end if

      if ( present(b) ) then
         select type ( g => b%in1 )
            type is ( integer )
               print *, g
            type is ( inner )
               print *, g%i
         end select
         select type ( g => b%in2 )
            type is ( integer )
               print *, g
            type is ( inner )
               print *, g%i
         end select
      end if

   end subroutine


end module

program valueOptionalDummyArg003
   use m

   type(base) :: b1
   class(base), allocatable :: b2
   type(base), pointer :: b3

   integer, target :: i1 = 100
   type(inner), target :: in1

   in1 = inner(1000)
   b1 = base(10, i1)
   call foo ( b1 )

   select type ( g => b1%in1 )
      type is ( integer )
         print *, g
      type is ( inner )
         print *, g%i
   end select
   select type ( g => b1%in2 )
      type is ( integer )
         print *, g
      type is ( inner )
         print *, g%i
   end select

   allocate ( b2, source = base(in1, i1) )

   call foo ( b2 )
   select type ( g => b2%in1 )
      type is ( integer )
         print *, g
      type is ( inner )
         print *, g%i
   end select

   select type ( g => b2%in2 )
      type is ( integer )
         print *, g
      type is ( inner )
         print *, g%i
   end select

   allocate ( b3, source = base(i1,in1) )

   call foo ( b1, b3 )

   select type ( g => b1%in1 )
      type is ( integer )
         print *, g
      type is ( inner )
         print *, g%i
   end select
   select type ( g => b1%in2 )
      type is ( integer )
         print *, g
      type is ( inner )
         print *, g%i
   end select

   select type ( g => b3%in1 )
      type is ( integer )
         print *, g
      type is ( inner )
         print *, g%i
   end select

   select type ( g => b3%in2 )
      type is ( integer )
         print *, g
      type is ( inner )
         print *, g%i
   end select

end program
