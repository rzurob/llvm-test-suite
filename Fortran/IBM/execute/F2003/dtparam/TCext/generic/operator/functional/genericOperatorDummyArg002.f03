! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/operator/functional/genericOperatorDummyArg002.f
! opt variations: -qnol -qnodeferredlp

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator
!*
!*  DESCRIPTION                : operator: non-poly pointer or allocatable dummy arguments being the operand
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

program genericOperatorDummyArg002
   use m

   type(base(:,4)), pointer :: b1
   type(base(:,4)), allocatable, target :: b2

   allocate ( b1, source = base(20,4) ( 100 ))
   b1 = add ( b1, b2 )
   print *, b1%i

   deallocate ( b2 )

   b1 = base(20,4)(b1%i/110)

   b2 = add ( b1, b2 )
   print *, b2%i

   b2 = add ( add ( b1, b2 ), b2 )
   print *, b2%i

   b1 => b2
   b1 = add ( b1, b2 )

   print *, b1%i, b2%i

   contains

      type(base(:,4)) function add(a, b)
         type(base(:,4)), intent(in), pointer :: a
         type(base(:,4)), intent(inout), allocatable  :: b

         pointer :: add

         print *, 'add'

         if ( .not. allocated  ( b ) ) allocate ( b, source = base(20,4)(1000) )

         allocate ( add , source = a + b )

      end function

end program

type(base(20,4)) function badd ( a, b )
   use m, only: base
   class(base(*,4)), intent(in) :: a
   type(base(*,4)), intent(in) :: b

   badd%i = a%i + b%i

   print *, 'badd'

end function
