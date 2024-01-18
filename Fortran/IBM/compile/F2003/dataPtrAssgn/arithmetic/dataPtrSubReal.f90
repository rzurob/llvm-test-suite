!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSubReal.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - defined assignment for LHS of a derivedtype with a pointer component
!*    & RHS of type real with allocatable attr
!* - the pointer component associated with the 2nd arg in the sub for defined =
!* - This is diagnostic TC. Pls see 327586 for details
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

  type extend
      class(*), pointer :: p(:)
  end type

  interface assignment(=)
        module procedure defAssgn2
  end interface

  contains
       subroutine defAssgn2(out, in)
           class(extend), intent(inout) :: out
           real, target, allocatable, intent(in) :: in(:)

           if ( .not. allocated(in)) stop 11

	   out%p(ubound(in,1):) => in(ubound(in,1):lbound(in,1):-2)
       end subroutine
end module
