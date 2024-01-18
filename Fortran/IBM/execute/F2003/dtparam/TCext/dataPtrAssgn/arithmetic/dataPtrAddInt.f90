! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/F2003/dataPtrAssgn/arithmetic/dataPtrAddInt.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

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
!* - data-target is a type bound procedure name with deferred binding-attr
!*
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type, abstract :: base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class(*), pointer :: p(:,:)
	contains
	    procedure(func), deferred :: pp
    end type

    type, extends(base) :: child(n2)    ! (4,20,5)
        integer, len  :: n2
        integer(k1) :: tar(n2,n2)

      	contains
	    procedure :: pp
    end type

    interface
	function func(arg)
	    import
	    integer, pointer :: func(:,:)
	    class(base(4,*)), intent(in) :: arg
	end function

        function pp(arg)
	    import
	    integer, pointer :: pp(:,:)
            class(child(4,*,*)), intent(in) :: arg
	end function
    end interface
end module

program main

    use m

    class(child(4,:,:)), allocatable :: b1

    allocate(b1, source = child(4,20,5)(null(),reshape((/ (i,i=1,25) /), (/5,5/))))

    b1%p(4:,5:) => b1%pp()

    if ( .not. associated(b1%p)) error stop 1
    if ( any (lbound(b1%p) .ne. (/4,5/))) error stop 2
    if ( any (ubound(b1%p) .ne. (/8,9/))) error stop 3

    select type (x => b1%p)
	type is (integer)
	    if ( any( x+x(8:4:-1,9:5:-1) .ne. 26 )) error stop 4
	class default
	    stop 5
    end select
end program

        function pp(arg)
	    use m, only : child
	    integer, pointer :: pp(:,:)
            class(child(4,*,*)), intent(in) :: arg
	    allocate(pp(size(arg%tar,1), size(arg%tar,2)), source = arg%tar)
	end function
