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
!*  SECONDARY FUNCTIONS TESTED : Mix generic type bounds
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : mix add, subtract, and assignment in a type with containers
!*
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module myint

   type int
      integer :: i
      contains

         procedure, pass, private :: add_int_int
         procedure, pass, private :: sub_int_int
         procedure, pass, private :: mul_int_int
         procedure, pass, private :: as_int_int
         procedure, pass, private :: sqrt_int

         generic :: operator(+) => add_int_int
         generic :: operator(-) => sub_int_int
         generic :: operator(*) => mul_int_int
         generic :: operator(.sqrt.) => sqrt_int
         generic :: assignment(=) => as_int_int

   end type

   contains

      subroutine as_int_int(a,b)
         class(int), intent(out) :: a
         class(int), intent(in)  :: b

         a%i = b%i

      end subroutine

      type(int) function add_int_int ( a, b )
         class(int), intent(in) :: a
         class(int), intent(in) :: b

         add_int_int%i = a%i + b%i

      end function

      type(int) function mul_int_int ( a, b )
         class(int), intent(in) :: a
         class(int), intent(in) :: b

         mul_int_int%i = a%i * b%i

      end function

      type(int) function sqrt_int ( a )
         class(int), intent(in) :: a

         sqrt_int%i = sqrt( real(a%i) )

      end function

      type(int) function sub_int_int ( a, b )
         class(int), intent(in) :: a
         class(int), intent(in) :: b

         sub_int_int%i = a%i - b%i

      end function

end module


module pt
   use myint

   type point
      type(int) :: x
      type(int) :: y
      contains
         procedure :: dist

         procedure, pass, private :: add_pt_pt
         procedure, pass, private :: sub_pt_pt
         procedure, pass, private:: as_pt_pt

         generic :: operator(+) => add_pt_pt
         generic :: operator(-) => sub_pt_pt
         generic :: assignment(=) => as_pt_pt

         generic :: distance => dist

   end type

   type, extends(point) :: cpoint
      character(3) :: color
      contains
         procedure, pass, private:: as_pt_pt => as_cpt_pt
   end type

   contains

      subroutine as_pt_pt(a,b)
         class(point), intent(out) :: a
         class(point), intent(in)  :: b

         a%x = b%x
         a%y = b%y

      end subroutine

      subroutine as_cpt_pt(a,b)
         class(cpoint), intent(out) :: a
         class(point), intent(in)  :: b

         select type ( b )
            type is ( cpoint )
               a%point = b%point
               a%color = b%color
            type is ( point )
               a%point = b
         end select

      end subroutine

      type(point) function add_pt_pt ( a, b )
         class(point), intent(in) :: a
         class(point), intent(in) :: b

         add_pt_pt%x = a%x + b%x
         add_pt_pt%y = a%y + b%y

      end function

      type(point) function sub_pt_pt ( a, b )
         class(point), intent(in) :: a
         class(point), intent(in) :: b

         sub_pt_pt%x = a%x - b%x
         sub_pt_pt%y = a%y - b%y

      end function

      real function dist ( a, b )
         class(point), intent(in) :: a, b

         associate( g => .sqrt.( ( a%x - b%x )*( a%x - b%x ) + ( a%y - b%y )*( a%y - b%y ) ) )
            dist = g%i
         end associate

      end function
end module

program genericMix003
   use pt

   class(point), allocatable :: p1, p2
   class(point), pointer :: p3

   real :: r

   allocate ( p1, source = point ( int(10), int(20) ) )
   allocate ( p2, source = cpoint ( int(100), int(200), 'RED' ) )

   allocate ( p3 )

   p3 = p1 + p2 - p1 - p2 + p1 + p2
   print *, p3%x%i, p3%y%i

   r = p1%dist(p2)
   print *, r
   r = p3%dist(p2)
   print *, r
   r = p1%dist(p3)
   print *, r

   deallocate ( p3 )
   allocate ( cpoint :: p3 )

   p3 = p2
   select type (p3)
      type is (cpoint)
         print *, p3%x%i, p3%y%i, p3%color
   end select

   p3 = p1 - p2 + p1 + p2
   select type (p3)
      type is (cpoint)
         print *, p3%x%i, p3%y%i, p3%color
   end select

end program
