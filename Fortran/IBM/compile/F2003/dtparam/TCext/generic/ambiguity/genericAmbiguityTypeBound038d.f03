! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/ambiguity/genericAmbiguityTypeBound038d.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : ambiguious generic interfaces
!*
!*  DESCRIPTION                : ambiguous with optional args, one procedure has more arguments than the other
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

module genericName

   type b1(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: i
      contains
         procedure, nopass :: twoargs1
         generic :: twoargs => twoargs1
   end type

   type, extends(b1) :: c1    ! (4)
      contains
         procedure, nopass :: twoargs2
         generic :: twoargs => twoargs2
   end type

   contains

      subroutine twoargs1(x, y)
         class(b1(4)) :: x
         type(c1(4))  :: y

         print *, 'twoargs1'

      end subroutine

      subroutine twoargs2(x, y, z)
         type(c1(4)) :: x
         type(c1(4)) :: y
         class(b1(4)), optional, intent(in) :: z

         print *, 'twoargs2'

      end subroutine

end module

program genericAmbiguityTypeBound038d
end program
