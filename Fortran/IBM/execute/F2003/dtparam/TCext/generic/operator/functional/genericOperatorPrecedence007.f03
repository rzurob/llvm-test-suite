! GB DTP extension using:
! ftcx_dtp -qck /tstdev/F2003/generic/operator/functional/genericOperatorPrecedence007.f
! opt variations: -qnock

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Operator: Binary Operators (numeric and character operators)
!*                                         Try // and +
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

   type base(k1,n1)    ! (1,4)
      integer, kind             :: k1
      integer, len              :: n1
      character(kind=k1,len=n1) :: c
      contains
         procedure :: add
         procedure :: con
         generic :: operator(+) => add
         generic :: operator(//) => con
  end type

   contains

   function add (a, b)
      type(base(1,4)) :: add
      class(base(1,*)), intent(in) :: a, b

      add%c(1:2) = a%c(1:2)
      add%c(3:4) = b%c(1:2)

      print *, 'add'
   end function

   function con (a, b)
      type(base(1,4)) :: con
      class(base(1,*)), intent(in) :: a, b

      con%c(1:2) = a%c(1:2)
      con%c(3:4) = b%c(3:4)

      print *, 'concat'

   end function

end module

program genericOperatorPrecedence007
   use m

   type(base(1,4)) b1, b2

   b1 = base(1,4) ('abcd')
   b2 = base(1,4) ('ABCD')

   b1 = b1 + b2
   print *, b1%c

   b2 = b1 // b2
   print *, b2%c

   b1 = b1 // b2 + b1
   print *, b1%c

   b2 = b2 // b2 // b2 + b2
   print *, b2%c

end program