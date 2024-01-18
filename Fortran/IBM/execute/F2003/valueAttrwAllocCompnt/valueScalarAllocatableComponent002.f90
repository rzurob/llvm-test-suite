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
!*                                 - type: intrinsic scalar allocatable components
!*                                 - actual arg: polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
!*                                 - dummy arg: polymorphic with value attribute
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
      character(len=5), allocatable :: c
      contains

         procedure, pass :: geti
         procedure, pass :: getc

   end type

   type, extends(base) :: child
      real, allocatable :: r
      contains

         procedure, pass :: getr

   end type

   class(child), pointer :: c2

   interface
      subroutine foo( dtv )
         import base
         type(base), value :: dtv
      end subroutine
   end interface

   interface
      subroutine bar( dtv )
         import child
         type(child), value :: dtv
      end subroutine
   end interface

   contains

      integer function geti ( a )
         class(base), intent(in) :: a
         geti = a%i
      end function

      character(5) function getc ( a )
         class(base), intent(in) :: a
         getc = a%c
      end function

      real function getr ( a )
         class(child), intent(in) :: a
         getr = a%r
      end function

end module

program valueScalarAllocatableComponent002
   use m

   type(base)  :: b1
   type(child) :: c1
   class(base), allocatable :: b2

   b1 = base(1,'abcde')
   c1 = child(2,'ABCDE', 10.0 )

   call foo( b1 )
   print *, b1%i, b1%c

   call bar( c1 )
   print *, c1%i, c1%c, c1%r

   allocate ( b2, source = base(3,'fghij' ) )

   call foo ( b2 )
   print *, b2%i, b2%c

   allocate ( c2, source = child ( 4, "FGHIJ", 20.0 ) )
   call bar ( c2 )

   print *, c2%i, c2%c, c2%r

   deallocate ( b2 )
   allocate ( b2, source = child ( 5, "house", 30.0 ) )

   call foo( b2 )
   select type ( b2 )
      type is ( child )
         print *, b2%i, b2%c, b2%r
   end select

end program


subroutine foo( dtv )
   use m, only: base
   type(base), value :: dtv

   print *, dtv%geti(), dtv%getc()
   dtv%i = -999
   dtv%c = 'xxxxx'
   print *, dtv%i, dtv%c

end subroutine

subroutine bar( dtv )
   use m, only: base, child
   type(child), value :: dtv

   print *, dtv%geti(), dtv%getc(), dtv%getr()
   dtv%i = -999
   dtv%c = 'xxxxx'
   dtv%r = -1.0
   print *, dtv%i, dtv%c, dtv%r

end subroutine

