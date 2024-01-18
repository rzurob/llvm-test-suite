! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/generic/syntax/C461/genericC461GenericName001.f
! opt variations: -ql

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : C461: Generic type bound with generic-name and do not
!*                                     specify pass object dummy argument
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
      integer(k1)   :: i = -999
   contains
      procedure, nopass, private :: typetotype
      generic :: nopassgeneric => typetotype, inttotype
      procedure, pass :: inttotype
   end type

   contains

   subroutine typetotype ( a, b )
      class(base(4)), intent(out) :: a
      class(base(4)), intent(in)  :: b

      a%i = b%i
   end subroutine

   subroutine inttotype ( a, b )
      class(base(4)), intent(out) :: a
      integer(4), intent(in)  :: b

      a%i = b
   end subroutine

end module

program genericC461GenericName001
end program
