! GB DTP extension using:
! ftcx_dtp -qk -qnol /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrSubReal.f
! opt variations: -qnok -ql

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrSubReal.f 
!*
!*  PROGRAMMER                 : Michelle Zhang
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!*
!* - defined assignment for LHS of a derivedtype with a pointer component
!*    & RHS of type real with allocatable attr
!* - the pointer component associated with the 2nd arg in the sub for defined = 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

  type extend(k1)    ! (4)
      integer, kind :: k1
      class(*), pointer :: p(:) 
  end type

  contains
       subroutine defAssgn2(out, in)
           class(extend(4)), intent(inout) :: out
           real, target, allocatable, intent(in) :: in(:)

           if ( .not. allocated(in)) stop 11  

	   out%p(ubound(in,1):) => in(ubound(in,1):lbound(in,1):-2)
       end subroutine
end module

program main
    use m
    type(extend(4))  e1
    real, target, allocatable :: r1(:)
    logical precision_r4

    allocate(r1(10), source=(/(real(i,4), i=1,10 )/) )

!    e1 = r1
    call defAssgn2(e1,r1)

    if ( .not. associated(e1%p, r1(10:1:-2))) stop 2
    if ( lbound(e1%p,1) /= 10 ) stop 3
    if ( ubound(e1%p,1) /= 14 ) stop 5 

    select type(x => e1%p)
	type is (real)
	    x = x - [1.0, 1.0, 1.0, 1.0, 1.0]
	    if (.not. precision_r4( x,(/(real(i-1), i=10,1,-2)/))) error stop 8_4 
	class default
	    stop 10
    end select	
end program
