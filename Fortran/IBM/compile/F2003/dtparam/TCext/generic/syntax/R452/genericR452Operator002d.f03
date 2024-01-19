! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/syntax/R452/genericR452Operator002d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : misc: defined-operator being some illegal operator
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
         generic :: operator(?) => a
         procedure, pass :: a
   end type

   type base1(n2,k2)    ! (20,4)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)      i
      contains
         generic :: operator(%) => b
         procedure, pass :: b
   end type

   contains

      type(base(20,4)) function a ( obj, passobj )
         class(base(*,4)), intent(in) :: obj
         class(base(*,4)), intent(in) :: passobj

         a%i = obj%i ** passobj%i

      end function

      type(base1(20,4)) function b ( obj, passobj )
         class(base1(*,4)), intent(in) :: obj
         class(base1(*,4)), intent(in) :: passobj

         b%i = obj%i ** passobj%i

      end function

end module

program genericR452Operator002d
end program
