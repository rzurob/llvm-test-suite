! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrANDLog.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrANDLog.f 
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
!* data-ptr is a component of a derived-type which is the type of a component 
!* of another derived type, multi-level of derived-types.  The last derived-type
!* contains an allocatable component which is the data-tar
!* 
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type A(k1)    ! (1)
	integer, kind        :: k1
	logical(k1), pointer :: p(:)
    end type

    type B(k2,k3)    ! (4,1)
        integer, kind :: k2,k3
        type(A(k3))   :: a1
    end type

    type :: C(k4,k5)    ! (4,1)
	integer, kind  :: k4,k5
	type(B(k4,k5)) :: b1
    end type

    type :: D(k6,k7)    ! (4,1)
	integer, kind  :: k6,k7
	type(C(k6,k7)) :: c1
    end type

    type E(k8,k9)    ! (4,1)
	integer, kind  :: k8,k9
	type(D(k8,k9)) :: d1
    end type

    type F(k10,k11)    ! (1,4)
	integer, kind             :: k10,k11
	type(E(k11,k10))          :: e1
	logical(k10), allocatable :: tar(:)
    end type 

end module

program main

    use m
    
    type(F(1,4)), target :: f1

    allocate(f1%tar(4), source = logical((/ .true.,.false., .true., .false. /), 1))
    f1%e1%d1%c1%b1%a1%p(1:4) => f1%tar(4:1:-1)

    if ( .not. associated(f1%e1%d1%c1%b1%a1%p,f1%tar(4:1:-1))) stop 1
    if ( any (lbound(f1%e1%d1%c1%b1%a1%p) .ne. (/1/))) stop 2 
    if ( any (ubound(f1%e1%d1%c1%b1%a1%p) .ne. (/4/))) stop 3 

    print *, f1%e1%d1%c1%b1%a1%p 
    print *, f1%e1%d1%c1%b1%a1%p .and. .true.

end program

