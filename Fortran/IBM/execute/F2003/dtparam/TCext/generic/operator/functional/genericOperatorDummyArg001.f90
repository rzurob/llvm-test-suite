! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorDummyArg001.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DESCRIPTION                : operator: non-poly dummy arguments being the operand
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

   type base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, pass :: badd
         generic :: operator(+) => badd
   end type

   interface
      type(base(20,4)) function badd ( a, b )
         import base
         class(base(*,4)), intent(in) :: a
         type(base(*,4)), intent(in) :: b
      end function
   end interface

end module

program genericOperatorDummyArg001
   use m

   type(base(20,4)) :: b1, b2
   type(base(:,4)), pointer :: b3

   b1 = add ( base(20,4)(100) , base(20,4)(50) )
   print *, b1%i

   b2 = add( add ( base(20,4)(200) , base(20,4)(300) ) , base(20,4)(400)  )
   print *, b2%i

   allocate ( base(20,4):: b3 )

   b3 = add( b1, b2 )
   print *, b3%i

   b2 = add ( b2, base(20,4)(200) )
   print *, b2%i

   contains

      type(base(20,4)) function add(a, b)
         type(base(*,4)), intent(in) :: a
         type(base(*,4)), intent(in)  :: b

         print *, 'add'
         add = a+ b

      end function

end program

type(base(20,4)) function badd ( a, b )
   use m, only: base
   class(base(*,4)), intent(in) :: a
   type(base(*,4)), intent(in) :: b

   badd%i = a%i + b%i

   print *, 'badd'

end function
