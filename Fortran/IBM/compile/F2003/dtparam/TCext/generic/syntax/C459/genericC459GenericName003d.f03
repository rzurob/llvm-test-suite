! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/syntax/C459/genericC459GenericName003d.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic name
!*
!*  DESCRIPTION                : C459: define generic TB with same generic name with different access-spec
!*                                     within same derived type ( inheritance )
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

   type base(k1)    ! (4)
      integer, kind :: k1
      integer(k1)   :: id
      contains
         procedure, nopass :: set1
         procedure, nopass :: set2
         procedure, nopass :: set4
         procedure, nopass :: set8
         generic :: set => set1
   end type

   type, extends(base) :: child    ! (4)
      contains
         generic :: set => set2
   end type

   type, extends(child) :: gen3    ! (4)
      contains
         generic, private :: set => set4, set8
   end type

   contains

      subroutine set1 ( a , i )
         class(base(4)), intent(inout) :: a
         integer(1), intent(in) :: i

      end subroutine

      subroutine set2 ( a , i )
         class(base(4)), intent(inout) :: a
         integer(2), intent(in) :: i

      end subroutine

      subroutine set4 ( a , i )
         class(base(4)), intent(inout) :: a
         integer(4), intent(in) :: i

      end subroutine

      subroutine set8 ( a , i )
         class(base(4)), intent(inout) :: a
         integer(8), intent(in) :: i

      end subroutine

end module

program genericC459Assignment003d
end program