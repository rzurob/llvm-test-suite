! GB DTP extension using:
! ftcx_dtp -ql /tstdev/F2003/mv_Alloc/ptrAssc/int8pntFromulmtTO.f
! opt variations: -qnol

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

    integer(8), target, allocatable :: i8(:)

    type A(n1,k1)    ! (20,8)
        integer, kind        :: k1
        integer, len         :: n1
        integer(k1), pointer :: p(:)
    end type

    class(*), target, allocatable :: ai(:)

    type(A(20,8)) :: aP

end module

    use m
    implicit integer*8 (i)
!    integer(8), pointer :: p(:)


    allocate(i8(100), source = (/ (i-1, i=1,100) /)  )
    allocate(integer*8:: ai(50))

    select type (ai)
        type is (integer*8)
            ai(:) = i8(::2)
    end select

    aP%p => i8

    call move_alloc(i8, ai)

    select type (ai)
        type is (integer*8)
            if ( .not. associated(aP%p, ai) ) error stop 21
            print *, aP%p(::2)
        class default
            stop 31
    end select

    end