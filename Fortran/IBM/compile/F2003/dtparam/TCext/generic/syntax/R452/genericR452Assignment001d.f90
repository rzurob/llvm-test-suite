! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/generic/syntax/R452/genericR452Assignment001d.f
! opt variations: -qnol

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : misc: assignment(some operator) not =
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
         generic :: assignment(+) => assgnmnt
         procedure, pass :: assgnmnt
   end type

   contains

      subroutine assgnmnt ( obj, passobj )
         class(base(*,4)), intent(out) :: obj
         class(base(*,4)), intent(in) :: passobj

         obj%i = passobj%i

      end subroutine

end module

program genericR452Assignment001d
end program
