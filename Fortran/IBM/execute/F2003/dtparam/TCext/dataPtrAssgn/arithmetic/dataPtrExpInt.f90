! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=none /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrExpInt.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrExpInt.f 
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
!* - data-ptr is a component of derived-type that defined a type bound defined =
!* - data pointer assgn applied in the subroutine for the type bound defined =
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m
  type base(k1,n1)    ! (4,20)
      integer, kind :: k1
      integer, len  :: n1
    class(*), pointer :: p(:)
    contains
       procedure :: defAssgn 
       generic :: assignment(=) => defAssgn
  end type

  type extend(k2,n2)    ! (4,20)
     integer, kind     :: k2
     integer, len      :: n2
     type(base(k2,n2)) :: comp 
     contains
       procedure :: defAssgn2 
       generic :: assignment(=) => defAssgn2
  end type

  contains
       subroutine defAssgn2(out, in)
	   class(extend(4,*)), intent(inout) :: out
	   integer, intent(in) :: in(:)

	   !out%comp = in
           call out%comp%defAssgn(in)
       end subroutine 
       subroutine defAssgn(out, in)
	   class(base(4,*)), intent(inout) :: out
	   integer, target, intent(in) :: in(:)
	   out%p(size(in):) => in(::2)
       end subroutine 
end module

program main
    use m

    type(extend(4,20))  e1

    integer, target :: ii(10) 

    ii =  (/(i, i=1,10 )/) 

    !e1 = ii 
    call e1%defAssgn2(ii)

    if ( .not. associated(e1%comp%p, ii(::2))) stop 1 
    if ( lbound(e1%comp%p,1) /= 10 ) stop 2
    if ( ubound(e1%comp%p,1) /= 14 ) stop 3 

    select type(x => e1%comp%p) 
	type is (integer)
	    if ( any( x**2 .ne. (/(i*i,i=1,9,2)/))) stop 6
	class default
	    stop 5
    end select

end program

