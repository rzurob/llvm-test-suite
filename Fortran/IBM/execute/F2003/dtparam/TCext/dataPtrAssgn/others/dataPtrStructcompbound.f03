! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv /tstdev/F2003/dataPtrAssgn/others/dataPtrStructcompbound.f
! opt variations: -ql -qdefaultpv

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
!* - components of DT as lb/ub
!* - lb/ub have different kind type parameter value
!* - data-pointer and data-target are of type logical
!*
!234567890123456789012345678901234567890123456789012345678901234567890

   module m
		type A(k1)    ! (1)
		    integer, kind            :: k1
		    integer(k1), allocatable :: lb
		end type

		type B(k2,k3)    ! (2,1)
            integer, kind        :: k2,k3
            integer(k2), pointer :: ub
            type(A(k3))          :: aa
		end type

        type, extends(A) :: C(k4)    ! (1,8)
            integer, kind :: k4
            integer(k4)   :: ub
        end type
   end module

   program main
        use m
		type(B(2,1)), pointer :: b1
        class(A(1)), allocatable :: a1

        logical, allocatable , target :: l1(:)
        logical, pointer :: p1(:,:)

        allocate(l1(50), source = (/(mod(i,2)==0 , i=1,50 ) /) )
	if ( .not. allocated(l1) ) error stop 3

        allocate(b1)
	if ( .not. associated(b1) ) error stop 4

        allocate(b1%ub, source = 24_2)
	if ( .not. associated(b1%ub) ) error stop 5

        allocate(b1%aa%lb, source = 8_1)
	if ( .not. allocated(b1%aa%lb) ) error stop 6

        allocate(C(1,8)::a1)
        if ( .not. allocated(a1) ) error stop 7

        select type(a1)
            type is (C(1,8))
                a1%ub = 13
                a1%lb = 17
                p1(b1%aa%lb:a1%ub, a1%lb:b1%ub) => l1(1_8:50_4)
            class default
                stop 13
        end select

	if ( .not. associated(p1)) error stop 9
	if (any(lbound(p1) .ne. (/8,17/))) error stop 5
	if (any(ubound(p1) .ne. (/13,24/))) error stop 7
        print *, p1

        End program