! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/syntax/C460/genericC460Operator002d.f
! opt variations: -ql

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
!*  DESCRIPTION                : C460: specific-binding exists in parent type,
!*                                     and parent type cannot use generic operator
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
      integer(k1)      i
      contains
         procedure, pass :: add
   end type

   type, extends(base) :: child    ! (4)
      contains
         generic :: operator(+) => add
   end type

   interface
      class(base(4)) function add(a, b)
         import base
         class(base(4)), intent(in) :: a, b
         allocatable :: add
      end function
   end interface

end module

program genericC460Operator002d
   use m

   type(base(4)) :: b1

   b1 = base(4)(123) + base(4)(246)

   b1 = b1 + base(4)(-369)


end program

class(base(4)) function add(a, b)
   use m, only: base
   class(base(4)), intent(in) :: a, b
   allocatable :: add

   allocate ( add, source = a )

   add%i = add%i + b%i
   
   print *, 'baseadd'

end function

