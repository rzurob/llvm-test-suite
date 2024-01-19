!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Value Attribute for derived type containing allocatable components
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : value attribute with derived type containing allocatable components
!*                                 - type: intrinsic scalar allocatable components
!*                                 - actual arg: non-polymorphic data arg (non-pointer non-allocatable, pointer, allocatable)
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
      character(len=5), allocatable :: c
      real, allocatable :: r

      contains

         procedure, pass :: geti
         procedure, pass :: getc
         procedure, pass :: getr

   end type

   type(base), pointer :: b3

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
         class(base), intent(in) :: a
         getr = a%r
      end function

      subroutine foo( dtv )
         type(base), value :: dtv

         print *, dtv%geti(), dtv%getc(), dtv%getr()
         dtv = base( -999, 'xxxxx', -999.0 )
         print *, dtv%geti(), dtv%getc(), dtv%getr()

      end subroutine

end module

program valueScalarAllocatableComponent001
   use m

   type(base) :: b1
   type(base), allocatable :: b2

   b1 = base(1,'house', 2.0)
   allocate ( b2, source = base(3,'mouse',4.0) )
   allocate ( b3, source = base(5,'apple',6.0) )

   call foo( b1 )
   print *, b1%i, b1%c, b1%r
   call foo( b2 )
   print *, b2%i, b2%c, b2%r
   call foo( b3 )
   print *, b3%i, b3%c, b3%r

end program
