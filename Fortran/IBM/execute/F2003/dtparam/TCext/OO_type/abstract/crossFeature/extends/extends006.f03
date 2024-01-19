! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/crossFeature/extends/extends006.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Extends keyword, ensure parent component accessibility
!*                                        parent component and parent's component have private accessibility
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
   type, abstract, private :: base(k1)    ! (4)
      integer, kind        :: k1
      integer(k1), private :: i = 5
   contains
      procedure, pass :: print => printbase
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      integer(k2)   :: r
   end type

   class(child(4,4)), allocatable :: c1

contains
   subroutine printbase(a)
      class(base(4)), intent(in) :: a
      print *, a%i
   end subroutine
end module


program extends006
   use m

   allocate(c1, source = child(4,4)(r=7))
   print *,c1%r
   call c1%print()

end program
