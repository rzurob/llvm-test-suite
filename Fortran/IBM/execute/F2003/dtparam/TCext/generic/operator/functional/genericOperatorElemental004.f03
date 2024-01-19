! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorElemental004.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: with elemental function with binary operator
!*                                         user defined operator defined in right operand
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

   type :: base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: x = -999
      contains
         procedure :: mul
   end type

   type, extends(base) :: c1    ! (20,4)
      contains
         generic :: operator(*) => mul
   end type

   type, extends(base) :: c2    ! (20,4)
   end type

   contains

      type(base(20,4)) elemental function mul (a, b)
         class(base(*,4)), intent(in) :: a
         class(base(*,4)), intent(in) :: b

         mul%x = a%x * b%x

      end function

end module

program genericOperatorElemental004
   use m

   type(base(20,4)) :: ans(3)

   class(base(:,4)), allocatable :: b1(:)

   type(c1(20,4)) :: c11(3) = (/ c1(20,4)(5), c1(20,4)(6), c1(20,4)(7) /)
   type(c2(20,4)) :: c21(3) = (/ c2(20,4)(15), c2(20,4)(16), c2(20,4)(17) /)

   allocate (  b1(3), source = c11 * (/ c2(20,4)(10), c2(20,4)(11),c2(20,4)(12) /)  )

   print *, b1%x

   ans = c21 * c11
   print *, ans

   ans = c11 * c21
   print *, ans

   ans = c21 * c11 * c11
   print *, ans

   ans = b1 * c11
   print *, ans

end program
