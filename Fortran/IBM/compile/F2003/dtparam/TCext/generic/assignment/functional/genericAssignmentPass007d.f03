! GB DTP extension using:
! ftcx_dtp /tstdev/F2003/generic/assignment/functional/genericAssignmentPass007d.f
! opt variations: -qck

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with assignment
!*
!*  DESCRIPTION                : assignment: pass-obj specified polymorphic assignment to different derived types
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
      integer(k1)   :: i = -999
      character(n1) :: c = 'xxx'
      contains
         procedure, pass(b) :: U_base
         procedure, pass(a) :: base_U
         generic :: assignment(=) => U_base, base_U
   end type

   contains

   subroutine U_base ( a, b )
      class(*), intent(out) :: a
      class(base(4,*)), intent(in)   :: b

   end subroutine

   subroutine base_U ( a, b )
      class(base(4,*)), intent(out) :: a
      class(*), intent(in)     :: b

   end subroutine

end module

program genericAssignmentPass007d
end program
