! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self /tstdev/F2003/generic/mix/genericMix003.f
! opt variations: -qnock -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : Mix generic type bounds
!*
!*  DESCRIPTION                : mix add, subtract, and assignment in a type with containers
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

   type int(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
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
         class(int(*,4)), intent(out) :: a
         class(int(*,4)), intent(in)  :: b

         a%i = b%i

      end subroutine

      type(int(20,4)) function add_int_int ( a, b )
         class(int(*,4)), intent(in) :: a
         class(int(*,4)), intent(in) :: b

         add_int_int%i = a%i + b%i

      end function

      type(int(20,4)) function mul_int_int ( a, b )
         class(int(*,4)), intent(in) :: a
         class(int(*,4)), intent(in) :: b

         mul_int_int%i = a%i * b%i

      end function

      type(int(20,4)) function sqrt_int ( a )
         class(int(*,4)), intent(in) :: a

         sqrt_int%i = sqrt( real(a%i) )

      end function

      type(int(20,4)) function sub_int_int ( a, b )
         class(int(*,4)), intent(in) :: a
         class(int(*,4)), intent(in) :: b

         sub_int_int%i = a%i - b%i

      end function

end module


module pt
   use myint

   type point(k2,n2)    ! (4,20)
      integer, kind    :: k2
      integer, len     :: n2
      type(int(n2,k2)) :: x
      type(int(n2,k2)) :: y
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

   type, extends(point) :: cpoint(k3,n3)    ! (4,20,1,3)
      integer, kind             :: k3
      integer, len              :: n3
      character(kind=k3,len=n3) :: color
      contains
         procedure, pass, private:: as_pt_pt => as_cpt_pt
   end type

   contains

      subroutine as_pt_pt(a,b)
         class(point(4,*)), intent(out) :: a
         class(point(4,*)), intent(in)  :: b

         a%x = b%x
         a%y = b%y

      end subroutine

      subroutine as_cpt_pt(a,b)
         class(cpoint(4,*,1,*)), intent(out) :: a
         class(point(4,*)), intent(in)  :: b

         select type ( b )
            type is ( cpoint(4,*,1,*) )
               a%point = b%point
               a%color = b%color
            type is ( point(4,*) )
               a%point = b
         end select

      end subroutine

      type(point(4,20)) function add_pt_pt ( a, b )
         class(point(4,*)), intent(in) :: a
         class(point(4,*)), intent(in) :: b

         add_pt_pt%x = a%x + b%x
         add_pt_pt%y = a%y + b%y

      end function

      type(point(4,20)) function sub_pt_pt ( a, b )
         class(point(4,*)), intent(in) :: a
         class(point(4,*)), intent(in) :: b

         sub_pt_pt%x = a%x - b%x
         sub_pt_pt%y = a%y - b%y

      end function

      real function dist ( a, b )
         class(point(4,*)), intent(in) :: a, b

         associate( g => .sqrt.( ( a%x - b%x )*( a%x - b%x ) + ( a%y - b%y )*( a%y - b%y ) ) )
            dist = g%i
         end associate

      end function
end module

program genericMix003
   use pt

   class(point(4,:)), allocatable :: p1, p2
   class(point(4,:)), pointer :: p3

   real :: r

   allocate ( p1, source = point(4,20) ( int(20,4)(10), int(20,4)(20) ) )
   allocate ( p2, source = cpoint(4,20,1,3) ( int(20,4)(100), int(20,4)(200), 'RED' ) )

   allocate ( point(4,20) :: p3 )

   p3 = p1 + p2 - p1 - p2 + p1 + p2
   print *, p3%x%i, p3%y%i

   r = p1%dist(p2)
   print *, r
   r = p3%dist(p2)
   print *, r
   r = p1%dist(p3)
   print *, r

   deallocate ( p3 )
   allocate ( cpoint(4,20,1,3) :: p3 )

   p3 = p2
   select type (p3)
      type is (cpoint(4,*,1,*))
         print *, p3%x%i, p3%y%i, p3%color
   end select

   p3 = p1 - p2 + p1 + p2
   select type (p3)
      type is (cpoint(4,*,1,*))
         print *, p3%x%i, p3%y%i, p3%color
   end select

end program
