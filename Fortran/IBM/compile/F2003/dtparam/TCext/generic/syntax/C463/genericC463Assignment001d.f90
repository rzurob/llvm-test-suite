! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/syntax/C463/genericC463Assignment001d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Assignment( )
!*
!*  DESCRIPTION                : C463: define as function
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
      integer(k1)      x,y
      contains
         procedure, pass :: mybassgn => b2b
         generic :: assignment(=) => mybassgn
   end type

   interface
      function b2b(a,b)
         import base
         class(base(*,4)), intent(out) :: a
         class(base(*,4)), intent(in) :: b
         type(base(20,4)) :: b2b
      end function
   end interface

   type base2(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)      i
      contains
         procedure, pass :: b2b2
         generic :: assignment(=) => b2b2
   end type

   contains

   function b2b2(a,b)

      class(base2(*,4)), intent(out) :: a
      class(base2(*,4)), intent(in) :: b
      type(base2(20,4)) :: b2b2

      b2b2%i = a%i

   end function

end module

end

function b2b(a,b)
   use m, only: base
   class(base(*,4)), intent(out) :: a
   class(base(*,4)), intent(in) :: b
   type(base(20,4)) :: b2b

   b2b%x = a%x
   b2b%y = b%y

end function
