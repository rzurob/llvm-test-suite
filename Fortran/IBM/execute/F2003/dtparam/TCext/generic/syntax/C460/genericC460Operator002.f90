! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/generic/syntax/C460/genericC460Operator002.f
! opt variations: -qnol -qnodeferredlp

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
!*  DESCRIPTION                : C460: specific-binding exists in parent type
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
      integer(k1)      i
      contains
         procedure, pass :: add
   end type

   type, extends(base) :: child    ! (20,4)
      contains
         generic :: operator(+) => add
   end type

   interface
      class(base(:,4)) function add(a, b)
         import base
         class(base(*,4)), intent(in) :: a, b
         allocatable :: add
      end function
   end interface

end module

program genericC460Operator002
   use m

   type(base(20,4)) :: c1

   c1 = child(20,4)(123) + base(20,4)(123)
   if ( c1%i /= 246 ) error stop 1_4

   c1 = c1 + child(20,4) (123)
   if ( c1%i /= 369 ) error stop 2_4

end program

class(base(:,4)) function add(a, b)
   use m, only: base
   class(base(*,4)), intent(in) :: a, b
   allocatable :: add

   allocate ( add, source = a )

   add%i = add%i + b%i

end function

