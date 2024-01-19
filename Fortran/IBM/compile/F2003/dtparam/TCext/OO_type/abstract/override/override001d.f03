! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/override/override001d.f
!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Type-bound procedure overriding
!*                               i) Either both shall have a passed-object or neither shall
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
         procedure(inf), deferred, nopass :: getid
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
      contains
         procedure, pass :: getid
   end type

   interface
      integer function inf(dtv)
         import base
         class(base(4)), intent(in) :: dtv
      end function
   end interface

   contains

      integer function getid(dtv)
         class(child(4,4)), intent(in) :: dtv
         getid=dtv%rid
      end function

end module

program override001d
end program
