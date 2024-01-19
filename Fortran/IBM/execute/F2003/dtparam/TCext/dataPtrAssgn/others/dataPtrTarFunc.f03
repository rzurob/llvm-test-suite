! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/others/dataPtrTarFunc.f
! opt variations: -qnol

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
!*  function return as data-target
!*  pointer is a component of DT
!*  element of array pointer as lb or ub of pointer or target
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        module m
		type A(n1,k1)    ! (20,4)
		    integer, kind        :: k1
		    integer, len         :: n1
		    integer(k1), pointer :: p(:,:)
		end type

		contains
                    function func(lb,ub)
	                integer, pointer :: func(:)
		        integer :: lb , ub

	                allocate(func(ub-lb+1), source=(/(i,i=lb,ub)/))
                    end function
        end module

        program main
                use m

		class(*), allocatable :: a1
		integer, pointer :: p(:)
		integer, target :: t(200)
		set_lb(i) = i*2 + 3

		allocate(A(20,4) :: a1)

	 	select type(a1)
		    type is (A(*,4))
			! bounds-remapping-list
			a1%p(20:25,set_lb(4):20) => func(101,200)

			if ( .not. associated(a1%p)) error stop 11
			print *, lbound(a1%p)
			print *, ubound(a1%p)
			print *, a1%p

			! bounds-spec-list
			p(a1%p(20,11):) => func(1,a1%p(25,20)/4)

			if ( .not. associated(p)) error stop 12
			print *, lbound(p)
			print *, ubound(p)
			print *, p
		    class default
			stop 31
		end select

        End program
