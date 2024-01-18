! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/syntax/C1112/genericC1112GenericName001d.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : C1112: generic-spec in use only statement shall not specify
!*                                      a generic binding
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
      integer(k1)   :: i
      contains
         procedure, pass :: add
         procedure, pass :: addint
         generic :: operator(+) => add, addint
   end type

   type base1(k2)    ! (4)
      integer, kind :: k2
      integer(k2)   :: i
      contains
         procedure, pass :: minus
         procedure, pass :: subtract
         generic :: operator(+) => minus, subtract
   end type

   contains

      type(base(4)) function add(a,b)
         class(base(4)), intent(in) :: a
         class(base(4)), intent(in) :: b

         add = base(4)(a%i+b%i)

      end function

      type(base(4)) function addint(a,b)
         class(base(4)), intent(in) :: a
         integer, intent(in) :: b

         addint= base(4)(a%i+b)

      end function

      type(base1(4)) function minus(a)
         class(base1(4)), intent(in) :: a

         minus = base1(4)(-1*a%i)

      end function

      type(base1(4)) function subtract(a,b)
         class(base1(4)), intent(in) :: a
         integer, intent(in) :: b

         subtract= base1(4)(a%i+b)

      end function

end module

program genericC1112GenericName001d
   use m, only: operator(+)
   use m, only: operator(-)
end program
