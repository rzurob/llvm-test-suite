! GB DTP extension using:
! ftcx_dtp -qk -ql /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrNeqvLog.f
! opt variations: -qnok -qnol

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
!* - data-ptr has bounds-remapping list; data-ptr of type class(*) and data-tar
!*   of type logical*8 are components of two different DTs where one DT with
!*   data-ptr as component is grand-child of the other DT with logical component!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type A(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
	class(*), pointer :: p(:,:,:)
    end type

    type, extends(A) :: B    ! (4,20)
    end type

    type, extends(B) :: C    ! (4,20)
    end type

    type, extends(C) :: D    ! (4,20)
    end type

    type, extends(D) :: E(k2)    ! (4,20,8)
	integer, kind            :: k2
	logical(k2), allocatable :: tar(:)
    end type

end module

program main

    use m

    type(E(4,20,8)), target :: e1

    allocate(e1%tar(4), source = logical((/ .true.,.false., .true., .false. /),8) )
    e1%p(3:3,0:0,2:5) => e1%tar(4:1:-1)

    if ( .not. associated(e1%p)) error stop 1
    if ( any (lbound(e1%p) .ne. (/3,0,2/))) error stop 2
    if ( any (ubound(e1%p) .ne. (/3,0,5/))) error stop 3

    select type(x=>e1%p)
	type is (logical*8)
	    print *, x
	    if ( x(3,0,2) .neqv. .false. ) error stop 4
	    if ( x(3,0,3) .neqv. .true. ) error stop 5
	    if ( x(3,0,4) .neqv. .false. ) error stop 6
	    if ( x(3,0,5) .neqv. .true. ) error stop 7
  	class default
            stop 9
    end select

end program
