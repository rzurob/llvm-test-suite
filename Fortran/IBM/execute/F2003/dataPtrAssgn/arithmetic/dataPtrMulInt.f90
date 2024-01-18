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
!* - data-ptr is a component of derived-type BASE that has type bound defined =
!* - type base is a component type of derived-type EXTEND
!* - derived-type intrinsic assignment for objects of type EXTEND
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  type base
    integer, pointer :: p(:)
    contains
       procedure :: defAssgn
       generic :: assignment(=) => defAssgn
  end type

  type extend
     type(base) :: bval
  end type

  contains
       subroutine defAssgn(out, in)
	   class(base), intent(inout) :: out
	   type(base), intent(in) :: in
       end subroutine
end module

program main
    use m

    type(extend)  e1, e2

    allocate(e2%bval%p(10), source = [ ( max(i**2, i*5), i=1,10) ])

    e1%bval%p(size(e2%bval%p):) =>  e2%bval%p(10:1:-1)

    if ( .not. associated(e1%bval%p, e2%bval%p(10:1:-1))) stop 1
    if ( lbound(e1%bval%p,1) /= 10 ) stop 2
    if ( ubound(e1%bval%p,1) /= 19 ) stop 3

    print *, e2%bval%p
    print *, e1%bval%p

    e1 = e2

    print *, e2%bval%p * 2
    print *, e1%bval%p * (/ (2, i=1,10 ) /)

end program

