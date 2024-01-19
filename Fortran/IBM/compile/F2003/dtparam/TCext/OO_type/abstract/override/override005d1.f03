! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/override/override005d1.f
!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Type-bound procedure overriding
!*                               v) passed-object dummy arguments, if any, shall correspond by name and position
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module n

   type, abstract :: base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
      contains
         procedure(inf), deferred, pass(j) :: setid
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
      contains
         procedure, pass(dtv):: setid
   end type

   interface
      subroutine inf(j,dtv)
         import base
         class(base(4)), intent(inout) :: j
         integer, intent(in) :: dtv
      end subroutine
   end interface

   contains

      subroutine setid(j,dtv)
         class(child(4,4)), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine

end module

program override005d1
end program
