! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/abstract/override/override007d1.f
!*  ===================================================================
!*
!*  DATE                       : 05/26/2005
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: Type-bound procedure overriding
!*                               vii)either shall be subroutines or both shall be functions with the same rsult characteristics
!*                                     - one is function, the other is function with another return type
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
         procedure(inf), deferred, pass :: setid
   end type

   type, extends(base) :: child(k2)    ! (4,4)
      integer, kind :: k2
      real(k2)      :: rid
      contains
         procedure, pass:: setid
   end type

   interface
      integer function inf(dtv,j)
         import base
         class(base(4)), intent(inout) :: dtv
         integer, intent(in) :: j
      end function
   end interface

   contains

      real function setid(dtv,j)
         class(child(4,4)), intent(inout) :: dtv
         integer, intent(in) :: j
         setid = dtv%rid
      end function

end module

program override007d
end program
