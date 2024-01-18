! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=self /tstdev/F2003/generic/operator/functional/genericOperatorStructConstr001.f
! opt variations: -ql -qreuse=none

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Scalar to Scalar with structure constructors
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: x = -99
      integer(k1)   :: y = -99
      contains
         procedure :: badd
         procedure :: bsub
         generic :: operator (+) => badd
         generic :: operator (-) => bsub
   end type

   interface
      type(base(4)) function bsub (a, b)
         import base
         class(base(4)), intent(in) :: a,b
      end function
   end interface

   interface
      type(base(4)) function badd (a, b)
         import base
         class(base(4)), intent(in) :: a,b
      end function
   end interface

end module

type(base(4)) function badd (a, b)
   use m, only: base
   class(base(4)), intent(in) :: a,b

   badd%x = a%x + b%x
   badd%y = a%y + b%y

end function

type(base(4)) function bsub (a, b)
   use m, only: base
   class(base(4)), intent(in) :: a,b
   bsub%x = a%x - b%x
   bsub%y = a%y - b%y
end function

program genericOperatorStructConstr001
   use m

   type(base(4)) :: b1
   class(base(4)), pointer :: b2

   b1 = ( base(4)(100, 101) + base(4) ( 200, 201 ) )

   allocate ( b2, source = ( base(4)() + base(4) ( 99 , 99 ) ) )

   if ( ( b1%x /= 300 ) .or. ( b1%y /= 302 ) ) error stop 1_4
   if ( ( b2%x /= 0 ) .or. ( b2%y /= 0 ) )     error stop 2_4

   nullify ( b2 )

   b1 = ( base(4) (y = 100 ) - base(4) ( x= -200 ) )
   if ( ( b1%x /= 101 ) .or. ( b1%y /= 199 ) ) error stop 3_4

end
