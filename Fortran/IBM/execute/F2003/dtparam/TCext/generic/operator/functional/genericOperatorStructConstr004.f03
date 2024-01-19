! GB DTP extension using:
! ftcx_dtp -ql -qnodefaultpv -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorStructConstr004.f
! opt variations: -qnol -qdefaultpv -qnodeferredlp -qreuse=self

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar to Scalar with structure constructors within structure constructor
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
      integer(k1)   :: j = -999
      contains
         procedure :: iadd
         generic :: operator(+) => iadd
   end type

   type base(n2,k2)    ! (20,4)
      integer, kind      :: k2
      integer, len       :: n2
      integer(k2)        :: x = -999
      type(inner(n2,k2)) :: i = inner(20,k2)()
      contains
         procedure :: badd
         generic :: operator (+) => badd
   end type

   interface
      type(base(20,4)) function badd (a, b)
         import base
         class(base(*,4)), intent(in) :: a,b
      end function
   end interface

   contains

      type(inner(20,4)) function iadd ( a, b ) result(abc)
         class(inner(*,4)), intent(in) :: a, b

         abc%j = a%j + b%j

      end function

end module

type(base(20,4)) function badd (a, b)
   use m, only: base
   class(base(*,4)), intent(in) :: a,b

   badd%x = a%x + b%x
   badd%i = a%i + b%i

end function

program genericOperatorStructConstr004
   use m

   type(base(:,4)), pointer :: b1
   type(base(20,4)), target :: b2

   b2 = base(20,4)() + base(20,4)()
   b1 => b2

   if ( ( b1%x /= -1998 ) .or. ( b1%i%j /= -1998 )      .or. &
        ( b2%x /= -1998 ) .or. ( b2%i%j /= -1998 ) )    error stop 1_4

   allocate ( b1 , source = base(20,4)( 100, inner(20,4)( 999 ) ) + base(20,4)( 200 ) )

   if ( ( b1%x /= 300 ) .or. ( b1%i%j /= 0 ) )          error stop 2_4

   b2 = base(20,4) ( 10, inner(20,4)(20) ) + base(20,4) (i = inner(20,4)(40) , x = 30 )
   if ( ( b2%x /= 40 ) .or. ( b2%i%j /= 60 ) )          error stop 3_4

end
