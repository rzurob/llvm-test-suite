! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/dataPtrAssgn/intrinsic/dataPtrUnpackInt.f
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
!* - data-pointer, type class(*), a component of DT
!* - data-target, type integer(4), a component of same DT as data-pointer
!* - DT contains third component of type integer*8, with allocatable attri
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    type base(n1,k1,k2)    ! (10,4,8)
        integer, kind            :: k1,k2
        integer, len             :: n1
        class(*), pointer :: p(:)
        integer(k1)              :: t4(n1)
        integer(k2), allocatable :: t8(:)
    end type

        type(base(10,4,8)),target :: b

        b%t4 = (/( i,i=1,10)/)

        b%p(1:) => b%t4

        if ( .not. associated(b%p, b%t4)) stop 5
        if ( lbound(b%p,1) /= 1 ) stop 7
        if ( ubound(b%p,1) /= 10 ) stop 9

        select type(x => b%p)
            type is (integer)
                print *, x
                print *, unpack(x,mod(x,2)==0,x(10:1:-1))
            class default
                stop 11
        end select

    end program
