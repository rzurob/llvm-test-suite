! GB DTP extension using:
! ftcx_dtp /tstdev/F2003/generic/syntax/C459/genericC459GenericName001d.f
! opt variations: -qck

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic name
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with different access-spec
!*                                     within the same derived type
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

   type base(k1,n1)    ! (4,3)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: id
      character(n1) :: name
      contains
         procedure, pass :: setid
         procedure, pass :: setname
         generic :: set => setid
         generic, private :: set => setname
   end type

   type base2(k2,n2)    ! (4,3)
      integer, kind :: k2
      integer, len  :: n2
      integer(k2)   :: id
      character(n2) :: name
      contains
         private
         procedure, nopass :: setid
         procedure, nopass :: setname
         generic :: set => setid
         generic, public :: set => setname
   end type

   contains

      subroutine setid ( a , i )
         class(base(4,*)), intent(inout) :: a
         integer, intent(in) :: i

      end subroutine

      subroutine setname ( a , c )
         class(base(4,*)), intent(inout) :: a
         character(3), intent(in) :: c

      end subroutine

end module

program genericC459Assignment001d
end program
