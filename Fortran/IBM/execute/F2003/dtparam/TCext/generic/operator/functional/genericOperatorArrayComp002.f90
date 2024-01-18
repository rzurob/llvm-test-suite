! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=self /tstdev/F2003/generic/operator/functional/genericOperatorArrayComp002.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=none

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
!*  DESCRIPTION                : Operator: Scalar with array components
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


module m

   type inner(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
   end type

   type base(k2,n2)    ! (4,20)
      integer, kind      :: k2
      integer, len       :: n2
      type(inner(n2,k2)) :: in(4)
      contains
         procedure, pass :: add
         generic :: operator ( + ) => add
   end type

   interface operator ( + )
      module procedure iadd
   end interface

   contains

   type(base(4,20)) function add ( a, b )
      class(base(4,*)), intent(in) :: a, b

      add%in = a%in + b%in

   end function

   type(inner(20,4)) function iadd ( a, b )
      class(inner(*,4)), intent(in), dimension(4) :: a,b
      dimension :: iadd(4)

      iadd%i = a%i + b%i

   end function

end module

program genericOperatorArrayComp002
   use m

   type(base(4,20)) :: b1, b2

   b1 = base(4,20) ( (/ inner(20,4)(1), inner(20,4)(2), inner(20,4)(3), inner(20,4)(4) /) )
   b2 = base(4,20) ( (/ inner(20,4)(11), inner(20,4)(12), inner(20,4)(13), inner(20,4)(14) /) )

   b1 = b1 + b2

   print *, b1%in

   b2 = b1 + b2 + b1 + b2

   print *, b2%in
   print *, base(4,20) ( (/ inner(20,4)(1), inner(20,4)(2), inner(20,4)(3), inner(20,4)(4) /) ) + b1 + base(4,20) ( (/ inner(20,4)(-1), inner(20,4)(-2), inner(20,4)(-3), inner(20,4)(-4) /) ) + b2

end program
