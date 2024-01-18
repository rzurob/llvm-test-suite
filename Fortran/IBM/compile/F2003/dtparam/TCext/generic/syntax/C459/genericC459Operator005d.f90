! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/F2003/generic/syntax/C459/genericC459Operator005d.f
! opt variations: -qnol -qreuse=base

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : C459: base type has public generic tb, and child type has private
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
         generic, public :: operator(*) => times_i
         procedure, pass  :: times_i => basetimesi
   end type

   contains

      function basetimesi ( passobj, int)
         class(base(*,4)), intent(in) :: passobj
         integer, intent(in) :: int
         type(base(20,4)) :: basetimesi
         basetimesi%i = passobj%i * int
      end function

end module

module n
   use m, only: base

   type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
      integer, kind :: k2
      integer, len  :: n2
      real(k2)      :: r = 0.0
      contains
         generic, private :: operator(*) => timeschild_i
         procedure, pass, private :: timeschild_i => childtimesi
   end type

   contains

      function childtimesi ( passobj, int )
         class(child(*,4,*,4)), intent(in) :: passobj
         real, intent(in) :: int
         type(child(20,4,20,4)) :: childtimesi
         childtimesi%i = passobj%i * int
         childtimesi%r = passobj%r * int

      end function

end module

program genericC459Operator005d
end program
