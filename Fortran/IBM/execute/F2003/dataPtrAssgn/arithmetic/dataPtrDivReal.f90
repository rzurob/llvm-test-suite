!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - subroutine defAssgn defines a defined assignment of having an object of
!*   a derivedtype as LHS and an object of intrinsic type as RHS. In the sub,
!*   the pointer component of the derived-type is associated with the 2nd arg.
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  type base
    real, pointer :: p(:)
  end type

   interface assignment(=)
        module procedure defAssgn
   end interface

  contains
       subroutine defAssgn(out, in)
           type(base), intent(inout) :: out
           real, target, intent(in) :: in(:)

           out%p(3:) => in
       end subroutine

end module

program main
    use m
    type(base)  e1
    real, target :: r1(10) = real( (/(i, i=-10,-1 )/) )
    logical precision_r4

    !e1 = r1
    call defAssgn(e1, r1)

    if ( .not. associated(e1%p, r1)) error stop 11
    if ( lbound(e1%p,1) /= 3 ) error stop 12
    if ( ubound(e1%p,1) /= 12 ) error stop 13
    if ( .not. precision_r4(e1%p/(-1.0), (/(real(i),i=10,1,-1)/))) error stop 15_4
end program
