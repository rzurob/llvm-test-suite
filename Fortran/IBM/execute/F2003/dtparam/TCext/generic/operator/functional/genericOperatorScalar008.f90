! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/F2003/generic/operator/functional/genericOperatorScalar008.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar with type within types
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

   type innerbase(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         generic, private :: operator(+) => inneradd
         procedure :: inneradd
   end type

   type base(k2)    ! (4)
      integer, kind       :: k2
      type(innerbase(k2)) :: b1
      contains
         generic :: operator(+) => add
         procedure :: add
   end type

   contains

   type(innerbase(4)) function inneradd ( a, b )
      intent(in) :: a, b
      class(innerbase(4)) :: a, b
      inneradd%i = a%i + b%i
   end function

   type(base(4)) function add ( a, b )
      intent(in) :: a, b
      class(base(4)) :: a, b
      add%b1 = a%b1 + b%b1
   end function

end module

program genericOperatorScalar008
   use m

   type(base(4)) :: b1, b2, b3
   allocatable :: b2
   pointer :: b3

   allocate ( b2, source = base(4)( innerbase(4)( 100 ) ) )
   b1 = base(4) ( innerbase(4)(10) ) +  base(4) ( innerbase(4)(200) )

   if ( b1%b1%i /= 210 )  error stop 1_4

   b2 = b2 + b1
   if ( b2%b1%i /= 310 )  error stop 2_4

   allocate ( b3, source = ( b1 + b2 + b2 ) )
   if ( b3%b1%i /= 830 )  error stop 3_4

end program
