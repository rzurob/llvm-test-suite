! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/F2003/mv_Alloc/ptrAssc/ulmTOpolyPntDTFrmcmp.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : 1.TO is of type class(*), a component of DT
!*                               2.FROM and pointer have the same declared type
!*                               3. FROM is a component of DT, non-poly
!*                               4. pointer is var, poly
!*                               defect 322504 322514
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (4)
        integer, kind :: k1
        class(*), allocatable :: a1(:)
    end type
end module

program main
use m

    type, extends(base) :: child    ! (4)
        type(base(k1)), allocatable :: b1 (:)
    end type

    type(child(4)), target :: c1
    type(base(4)), allocatable :: ll(:)
    class(base(4)), pointer  :: p(:)

    allocate(ll(2), source = (/ base(4)( (/21, 32 /) ), base(4)( (/56, 67/))  /))

    c1 = child(4)( a1= null(), b1 = ll )

    p => c1%b1

    call move_alloc(c1%b1, c1%a1)

    if ( allocated (c1%b1) ) error stop 11
    if ( .not. allocated(c1%a1)) error stop 13

    select type ( p )
        type is (base(4))
            select type ( x => p(1)%a1 )
                type is (integer)
                    if ( x(1) /= 21 ) error stop 21
                    if ( x(2) /= 32 ) error stop 23
                class default
                    stop 31
            end select
            select type ( x => p(2)%a1 )
                type is (integer)
                    if ( x(1) /= 56 ) error stop 25
                    if ( x(2) /= 67 ) error stop 27
                class default
                    stop 41
            end select
        class default
            stop 43
    end select

    select type ( x=> c1%a1)
        type is (base(4))
            if ( .not. associated(p, x)) error stop 61
	class default
            stop 63
    end select

    end