! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrORLog.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

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
!* - data-ptr and data-target are components of different level derived-types
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type A(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
	class(*), pointer :: p(:,:)
    end type

    type, extends(A) :: B(k2,n2)    ! (4,20,4,20)
        integer, kind :: k2
        integer, len  :: n2
    end type

    type, extends(B) ::C(k3,n3)    ! (4,20,4,20,4,20)
	integer, kind  :: k3
	integer, len   :: n3
	type(A(k3,n3)) :: a1
    end type

end module

program main

    use m

    type(C(4,:,4,:,4,:)), allocatable :: c1

    allocate(C(4,20,4,20,4,20) :: c1)

    allocate(c1%a1%p(2,2), source = reshape(logical((/ .true.,.false., .true., .true. /), 2), (/2,2/)))

    c1%p(1:,2:) => c1%a1%p

    if ( any (lbound(c1%p) .ne. (/1,2/))) error stop 2
    if ( any (ubound(c1%p) .ne. (/2,3/))) error stop 3

    select type(x=>c1%a1%p)
	type is (logical*2)
	    if ( any( x .or. eoshift(x, shift=1)  .neqv. reshape((/.true., .false., .true., .true. /), (/2,2/)))) error stop 5
  	class default
            stop 9
    end select

end program

