!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 03/17/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.9: structure constructor)
!                               Case: Data target is a function reference which
!                               returns a data pointer.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base (k)
        integer, kind :: k

        integer(k), pointer :: id
        procedure(genInt8), pointer, nopass :: genID
    end type

    contains

    integer(8) function genInt8 (i)
        integer, intent(in) :: i

        pointer :: genInt8

        if (mod(i,2) == 1) then
            allocate (genInt8, source=int(i,8))
        else
            nullify(genInt8)
        end if
    end function
end module

program dtparamConstr037
use m
    type (base(8)) b1(100)

    b1 = base(8)(null(), genInt8)

    do i = 1, 100
        if (associated(b1(i)%id) .or. &
            (.not. associated(b1(i)%genID, genInt8))) error stop 1_4

        b1(i) = base(8)(b1(i)%genID(i), genInt8)
    end do

    do i = 1, 100
        if (.not. associated(b1(i)%genID, genInt8)) error stop 2_4

        if (mod(i,2) == 0) then
            if (associated(b1(i)%id)) error stop 3_4
        else
            if (b1(i)%id /= i) error stop 4_4
        end if
    end do
end
