! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/operator/functional/genericOperatorElemental005.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: with elemental function with abstract types
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

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: x = -999
      contains
         generic :: operator(*) => mul
         procedure(inte), deferred :: mul
   end type

   type, extends(base) :: c1    ! (4)
      contains
         procedure :: mul => mc1
   end type

   interface
      type(c1(4)) elemental function inte (a, b)
         import base, c1
         class(base(4)), intent(in) :: a, b
      end function
   end interface

   contains

      type(c1(4)) elemental function mc1 (a, b)
         class(c1(4)), intent(in) :: a
         class(base(4)), intent(in) :: b

         mc1%x = a%x * b%x

      end function

end module

module n
   use m

   type, extends(c1) :: c2    ! (4)
   end type

end module

program genericOperatorElemental005
   use n

   class(base(4)), allocatable :: b1
   type(c1(4)) :: c11
   class(c1(4)), allocatable :: c12

   type(c2(4)) :: c21
   class(c2(4)), allocatable :: c22

   allocate ( b1, source = c1(4)(10) * c1(4)(20) )
   c11 = c1(4)(2) * c2(4)(20)

   allocate (c12, source = c11 * c2(4)(20) )

   print *, b1%x
   print *, c11%x
   print *, c12%x

   c21 = c2(4)(100)
   allocate ( c22, source = c2(4)(50))

   c11 = c21 * c22
   print *, c11%x
   deallocate ( c12 )
   allocate ( c12, source = c21 * c2(4)(5) * c22 )
   print *, c12%x


end program
