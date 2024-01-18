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
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Operator: Deferred specific binding
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

   type, abstract :: pt
      contains
         procedure(i), deferred :: subtr
         generic :: operator(-) => subtr
   end type

   type, extends(pt) :: onedpt
      integer :: x
      contains
         procedure :: subtr => subtr1d1d
   end type

   type, extends(pt) :: twodpt
      integer :: x, y
      contains
         procedure :: subtr => subtr2d2d
   end type

   interface
      class(pt) function i ( a, b )
         import pt
         class(pt), intent(in) :: a
         class(pt), intent(in) :: b
         pointer :: i
      end function
   end interface

   contains

   class(pt) function subtr1d1d ( a, b )
      class(onedpt), intent(in) :: a
      class(pt), intent(in) :: b
      pointer :: subtr1d1d

      select type ( b )
         type is ( onedpt )
            allocate ( subtr1d1d, source = onedpt ( x = ( a%x - b%x ) ) )
         type is ( twodpt )
            allocate ( subtr1d1d, source = onedpt ( x = ( a%x - b%x ) ) )
      end select

   end function

   class(pt) function subtr2d2d ( a, b )
      class(twodpt), intent(in) :: a
      class(pt), intent(in) :: b
      pointer :: subtr2d2d

      select type ( b )
         type is ( twodpt )
            allocate ( subtr2d2d, source = twodpt ( x = ( a%x - b%x ), y = ( a%y - b%y ) ) )
         type is ( onedpt )
            allocate ( subtr2d2d, source = twodpt ( x = ( a%x - b%x ), y = a%y ) )
      end select

   end function

end module

program genericOperatorDeferred002
   use m

   class(pt), allocatable     :: p0, p01, p11, p21
   class(onedpt), pointer     :: p1
   class(twodpt), allocatable :: p2

   allocate ( p0, source = onedpt(10) )
   allocate ( p1, source = onedpt(20) )
   allocate ( p2, source = twodpt(30,40) )

   allocate ( p01, source = ( p0 - p1 ) )

   select type ( p01 )
      type is ( onedpt )
         if ( p01%x /= -10 ) error stop 1_4
      class default
         error stop 2_4
   end select

   deallocate ( p01 )
   allocate ( p01, source = ( p0 - p2 ) )

   select type ( p01 )
      type is ( onedpt )
         if ( p01%x /= -20 ) error stop 3_4
      class default
         error stop 4_4
   end select

   deallocate ( p01 )
   allocate ( p01, source = ( p1 - p2 ) )

   select type ( p01 )
      type is ( onedpt )
         if ( p01%x /= -10 ) error stop 5_4
      class default
         error stop 6_4
   end select

   deallocate ( p01 )
   allocate ( p01, source = ( p2 - p1 ) )

   select type ( p01 )
      type is ( twodpt )
         if ( ( p01%x /= 10 ) .or. ( p01%y /= 40 ) )  error stop 7_4
      class default
         error stop 8_4
   end select

   allocate ( p11, source = ( p1 - onedpt ( 1000 ) ) )
   select type ( p11 )
      type is ( onedpt )
         if ( p11%x /= -980 ) error stop 9_4
      class default
         error stop 10_4
   end select

   allocate ( p21, source = ( p2 - onedpt ( 100 ) ) )
   select type ( p21 )
      type is ( twodpt )
         if ( ( p21%x /= -70 ) .or. ( p21%y /= 40 ) ) error stop 11_4
      class default
         error stop 12_4
   end select

   deallocate ( p21 )

   allocate ( p21 , source = ( p2 - twodpt(-30,-40)) )
   select type ( p21 )
      type is ( twodpt )
         if ( ( p21%x /= 60 ) .or. ( p21%y /= 80 ) ) error stop 13_4
      class default
         error stop 14_4
   end select

end program
