!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with (non-)polymorphic allocatable components
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
      type(inner), allocatable :: in
   end type

   contains

   subroutine foo ( a )
      type(base), value, asynchronous :: a

      write( 1, asynchronous="yes" ) a%in%i

   end subroutine

   subroutine bar ( a )
      type(base), asynchronous :: a

      read( 1, asynchronous="yes" ) a%in%i

   end subroutine

   subroutine badbar ( a )
      type(base), asynchronous, value :: a
      integer :: myid

      print *, 'inside badbar'
      read( 1, asynchronous="yes", id= myid ) a%in%i
      wait(1,id=myid)
      print *, a%in%i

   end subroutine

end module

program valueOptionalDummyArg002
   use m

   type(base) :: b1
   type(base), allocatable :: b2

   b1 = base(inner(100))
   allocate ( b2, source =  base(inner(1000)) )
   open ( 1, file="valueOptionalDummyArg002.1", asynchronous="yes", form="unformatted" )

   call foo ( b1 )
   b1 = base(inner(-999))
   print *, b1%in%i

   call foo ( b2 )
   b2 = base(inner(-999))
   print *, b2%in%i

   rewind 1

   call bar(b1)
   print *, 'after bar'
   print *, b1%in%i

   call badbar(b2)
   print *, 'after badbar'
   print *, b2%in%i

   close ( 1, status="delete" )

end program
