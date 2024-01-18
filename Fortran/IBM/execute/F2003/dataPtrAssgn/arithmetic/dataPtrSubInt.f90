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
!* - data-ptr is a component of a derived-type with defined = for it
!* - the type has final subroutine
!* - defect 320269,  finalization is disabled by defined assignment
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  type base
    integer, pointer :: p(:)
    contains
       final :: final4base
  end type

  type extend
     type(base) :: bval
     contains
        final :: final4extend
  end type

  interface assignment(=)
     module procedure assign_defa
  end interface

  contains
       subroutine final4base(arg)
          type(base), intent(inout) :: arg
          print *, "this is final of base "
       end subroutine
       subroutine final4extend(arg)
          type(extend), intent(inout) :: arg
          print *, "this is final of extend "
       end subroutine
       subroutine assign_defa(left,right)
          class(base), intent(inout) :: left
          class(base), intent(in) :: right
          print *, "this is defined assignment of base"
       end subroutine
end module

program main
    use m

    type(extend)  e1, e2

    allocate(e2%bval%p(10), source = [ ( max(i**2, i*5), i=1,10) ])

    e1 = e2

    e1%bval%p(size(e2%bval%p):) =>  e2%bval%p(10:1:-1)


    if ( .not. associated(e1%bval%p, e2%bval%p(10:1:-1))) stop 1
    if ( lbound(e1%bval%p,1) /= 10 ) stop 2
    if ( ubound(e1%bval%p,1) /= 19 ) stop 3
    print *, e1%bval%p - 100

end program

