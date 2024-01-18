! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C611/structComp002.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        Non-rightmost part-name a scalar object, with intrinsic assignment
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

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
   end type

end module

program structComp002
   use m

   type(child(4,4)) :: c1

   type(child(4,4)), target :: c11
   type(child(4,4)), target, allocatable :: c12
   class(child(4,4)), pointer :: c13

   c1%base = c11%base

   allocate (c12, source = child(4,4)(1,2.3) )
   c13 => c12

   c1%base = c12%base
   c1%base = c13%base

end program
