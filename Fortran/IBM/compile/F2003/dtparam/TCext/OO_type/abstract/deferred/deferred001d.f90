! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/deferred/deferred001d.f
!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Deferred Binding
!*                                  - define the same binding (between specific and deferred) within
!*                                    the same derived type
!*
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
      integer(k1)   :: id = -999
      contains
         procedure(inf), deferred, pass :: setid
         procedure, pass                :: setid => setbaseid
   end type

   interface
      subroutine inf(dtv,j)
         import base
         class(base(4)), intent(inout) :: dtv
         integer, intent(in) :: j
      end subroutine
   end interface

   contains

      subroutine setbaseid(dtv,j)
         class(base(4)), intent(inout) :: dtv
         integer, intent(in) :: j
         dtv%id = j
      end subroutine

end module

program deferred001d
end program
