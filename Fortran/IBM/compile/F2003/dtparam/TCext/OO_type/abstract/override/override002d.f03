! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/override/override002d.f
!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Type-bound procedure overriding
!*                               ii) If the overridden binding is pure then the overridding binding shall also be pure
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
      contains
         procedure(inf), deferred, pass :: getid
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
      contains
         procedure, pass :: getid
   end type

   interface
      pure subroutine inf(dtv)
         import base
         class(base(4)), intent(inout) :: dtv
      end subroutine
   end interface

   contains

      subroutine getid(dtv)
         class(child(4,4)), intent(inout) :: dtv
         dtv%id=10
      end subroutine

end module

program override002d
end program
