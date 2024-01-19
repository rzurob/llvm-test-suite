! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/C482/structConstr003.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: The derived-type-spec shall not specify an abstract type (C401)
!*                                        Structure Constructor as Array Constructor (for pointer/allocatable)
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

program structConstr003
   use m
   class(base(4)), allocatable, dimension(:) :: b1
   type(child(4,4)), pointer :: c1(:)
   class(*), allocatable :: u1(:)

   allocate(b1(3), source = (/ base(4)(2), base(4)(3), base(4)(4) /) )
   allocate(c1(2), source = (/ child(4,4)(base=base(4)(2),rid=5.5), child(4,4)(base=base(4)(3),rid=5.7) /) )
   allocate(u1(3), source = (/ base(4)(2), base(4)(3), base(4)(4) /) )
end program
