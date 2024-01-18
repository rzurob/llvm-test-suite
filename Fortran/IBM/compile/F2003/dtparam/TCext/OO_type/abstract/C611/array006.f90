! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C611/array006.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: If the rightmost part-name is of abstract type, data-ref shall be polymorphic. (C611)
!*                                        R614: structure-component is data-ref
!*                                        non-polymorphic abstract type data-ref assigned data, intrinsic assignment, and associate construct
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

program array006
   use m

   type(child(4,4)) :: c1(5)
   class(child(4,4)), allocatable, dimension(:) :: c2
   class(child(4,4)), pointer, dimension(:) :: c3

   allocate (c2(5), c3(4))

   c1%base = c2%base
   c1(2:5)%base = c3%base

   associate ( g => c1%base, h => c2%base )
      g = h
   end associate

end program
