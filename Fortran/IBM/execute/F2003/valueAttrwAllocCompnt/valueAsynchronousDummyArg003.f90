!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with unlimited-polymorphic allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable) with asynchronous attribute
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
      class(*), allocatable :: in
   end type

   contains

   integer function foo ( a )
      type(base), value, asynchronous :: a

      select type ( g => a%in )
         type is ( integer )
            write( 1, asynchronous="yes", iostat = foo ) g
         type is ( inner )
            write( 1, asynchronous="yes", iostat = foo ) g%i
      end select

   end function

   integer function bar ( a )
      type(base), asynchronous :: a

      select type  ( g => a%in )
         type is ( integer )
            read( 1, asynchronous="yes", iostat = bar ) g
         type is ( inner )
            read( 1, asynchronous="yes", iostat = bar  ) g%i
      end select

   end function

   integer function badbar ( a )
      type(base), asynchronous, value :: a
      integer :: myid

      print *, 'inside badbar'

      select type  ( g => a%in )
         type is ( integer )
            read( 1, asynchronous="yes", id= myid, iostat = badbar  ) g
            wait(1,id=myid)
            print *, g
         type is ( inner )
            read( 1, asynchronous="yes", id= myid, iostat = badbar ) g%i
            wait(1,id=myid)
            print *, g%i
      end select

   end function

end module

program valueOptionalDummyArg003
   use m

   type(base) :: b1
   type(base), allocatable :: b2

   integer :: i

   b1 = base(100)
   allocate ( b2, source =  base(inner(1000)) )
   open ( 1, file="valueOptionalDummyArg003.1", asynchronous="yes", form="unformatted" )

   i= foo ( b1 )
   if ( i /= 0 ) error stop 1_4
   b1 = base(-999)
   select type ( g => b1%in )
      type is ( integer )
         print *, g
   end select

   i= foo ( b2 )
   if ( i /= 0 ) error stop 1_4
   b2 = base(inner(-999))
   select type ( g => b2%in )
      type is ( inner )
         print *, g%i
   end select

   rewind 1

   i= bar(b1)
   if ( i /= 0 ) error stop 1_4
   print *, 'after bar'
   select type ( g => b1%in )
      type is ( integer )
         print *, g
   end select

   i= badbar(b2)
   if ( i /= 0 ) error stop 1_4
   print *, 'after badbar'
   select type ( g => b2%in )
      type is ( inner )
         print *, g%i
   end select

   close ( 1, status="delete" )

end program
