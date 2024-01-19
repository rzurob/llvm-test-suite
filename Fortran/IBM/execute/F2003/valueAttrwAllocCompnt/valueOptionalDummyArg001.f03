!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: derived type with intrinsic allocatable components
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

   type base
      integer, allocatable :: i
   end type

   type, extends(base) :: child
      real, allocatable :: r
   end type

   contains

   subroutine foo ( a, b )
      type(base), value, optional :: a
      type(base), value, optional :: b

      print *, 'foo:'
      if ( present(a) ) then
      	 print *, a%i
      	 a%i = -999
      	 print *, a%i
      end if

      if ( present(b) ) then
      	 print *, b%i
      	 b%i = -999
      	 print *, b%i
      end if

   end subroutine

   subroutine bar ( a, b )
      type(child), value, optional :: a
      type(child), value, optional :: b

      print *, 'bar:'
      if ( present(a) ) then
      	 print *, a%i, a%r
      	 a%i = -999
      	 a%r = -999.9999
      	 print *, a%i, a%r
      end if

      if ( present(b) ) then
      	 print *, b%i, b%r
      	 b%i = -999
      	 b%r = -999.9999
      	 print *, b%i, b%r
      end if

   end subroutine

end module

program valueOptionalDummyArg001
   use m

   type(base), pointer :: b1
   class(base), allocatable :: b2
   type(child), allocatable :: c1
   class(child), pointer :: c2

   allocate ( b1, source = base(100) )
   allocate ( b2, source = child(200,-999.9999) )
   allocate ( c1, source = child(10,15.0) )
   allocate ( c2, source = child(20,25.0) )

   call foo()
   call foo(b=b1)
   print *, b1%i
   call foo(b2)
   print *, b2%i
   call foo(b=b2,a=b1)
   print *, b1%i, b2%i

   call bar()
   call bar(b=c1)
   print *, c1%i, c1%r
   call bar(c2)
   print *, c2%i, c2%r
   call bar(b=c2,a=c1)
   print *, c1%i, c1%r, c2%i, c2%r

end program
