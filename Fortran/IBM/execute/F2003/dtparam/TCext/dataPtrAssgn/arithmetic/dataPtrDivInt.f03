! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=none /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrDivInt.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

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
!* - data-ptr is a component of derived-type that has a type bound defined =
!* - data pointer assgn applied in the subroutine for the type bound defined =
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  type base(n1,k1)    ! (20,4)
    integer, kind        :: k1
    integer, len         :: n1
    integer(k1), pointer :: p(:)
    contains
       procedure :: defAssgn
       generic :: assignment(=) => defAssgn
  end type

  type extend(k2,n2)    ! (4,20)
     integer, kind     :: k2
     integer, len      :: n2
     type(base(n2,k2)) :: bval
  end type

  contains
       subroutine defAssgn(out, in)
	   class(base(*,4)), intent(inout) :: out
	   type(base(*,4)), intent(in) :: in
	   out%p(2:) => in%p(::2)
       end subroutine
end module

program main
    use m

    type(extend(4,20))  e1, e2

    allocate(e2%bval%p(10), source = [ ( max(i**2, i), i=1,10) ])

    e1%bval%p(size(e2%bval%p):) =>  e2%bval%p(10:1:-1)

    if ( .not. associated(e1%bval%p, e2%bval%p(10:1:-1))) error stop 1
    if ( lbound(e1%bval%p,1) /= 10 ) error stop 2
    if ( ubound(e1%bval%p,1) /= 19 ) error stop 3

    e1 = e2

    if ( .not. associated(e1%bval%p, e2%bval%p(::2))) error stop 11
    if ( lbound(e1%bval%p,1) /= 2 ) error stop 12
    if ( ubound(e1%bval%p,1) /= 6 ) error stop 13

    if ( any ( e1%bval%p / (/(i, i=1,9,2 ) /) .ne. (/(i, i=1,9,2 ) /))) error stop 20

end program

