! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc021a1.f
!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: falloc021a1.f
! %VERIFY: falloc021a1.out:falloc021a1.vf
! %STDIN:
! %STDOUT: falloc021a1.out
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 09/16/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : ALLOCATE (pointer of multi-dimension as the
!                               function return result)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        integer(selected_int_kind(3)) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n2)    ! (4,20,4,20)
        integer, kind :: k2
        integer, len  :: n2
        real (selected_real_kind (6, 70)) :: value

        contains

        procedure :: print => printChild
    end type

    interface genData
        class (base(4,20)) function genBaseScalarPtr (id, value)
        import base, child
            integer, intent(in) :: id
            real, intent(in), optional :: value
            pointer genBaseScalarPtr
        end function

        class (base(4,20)) function genBasePtrRank1 (id, value, lb, ub)
        import base, child
            integer, intent(in) :: id, lb, ub
            real, intent(in), optional :: value
            pointer genBasePtrRank1(:)
        end function

        class (base(4,20)) function genBasePtrRank2 (id, value, lb, ub)
        import base, child
            integer, intent(in) :: id, lb(2), ub(2)
            real, intent(in), optional :: value
            pointer genBasePtrRank2(:,:)
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(4,*)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,*,4,*)), intent(in) :: b

        write (*, '(i5,f15.3)') b%id, b%value
    end subroutine
end module


class (base(4,20)) function genBaseScalarPtr (id, value)
    use m, only: base, child

    integer, intent(in) :: id
    real, intent(in), optional :: value
    pointer genBaseScalarPtr

    if (present(value)) then
        allocate (genBaseScalarPtr, source=child(4,20,4,20)(id, value))
    else
        allocate (genBaseScalarPtr, source=base(4,20)(id))
    end if
end function

class (base(4,20)) function genBasePtrRank1 (id, value, lb, ub)
    use m, only: base, child

    integer, intent(in) :: id, lb, ub
    real, intent(in), optional :: value
    pointer genBasePtrRank1(:)

    if (present(value)) then
        allocate (genBasePtrRank1(lb:ub), source=(/(child(4,20,4,20)(id+i, value+i), i=lb,ub)/))
    else
        allocate (genBasePtrRank1(lb:ub), source=(/(base(4,20)(id+i), i=lb,ub)/))
    end if
end function

class (base(4,20)) function genBasePtrRank2 (id, value, lb, ub)
    use m, only: base, child

    integer, intent(in) :: id, lb(2), ub(2)
    real, intent(in), optional :: value
    pointer genBasePtrRank2 (:,:)

    if (present(value)) then
        allocate (genBasePtrRank2(lb(1):ub(1), lb(2):ub(2)), &
                source=reshape((/((child(4,20,4,20)(id+i+j, value+i+j), i=lb(1), ub(1)), &
                        j=lb(2),ub(2))/), ub-lb+1))

    else
        allocate (genBasePtrRank2(lb(1):ub(1), lb(2):ub(2)), &
                source=reshape((/((base(4,20)(id+i+j), i=lb(1), ub(1)), &
                        j=lb(2),ub(2))/), ub-lb+1))

    end if
end function


program falloc021a1
use m
    class (base(4,20)), pointer :: b_ptr, b1_ptr(:), b2_ptr(:,:)


    !! test using child type as dynamic type
    b_ptr => genData (1, 1.1)

    b1_ptr => genData (2, 2.1, -1, 0)

    b2_ptr => genData (3, 3.2, (/0, 0/), (/2,1/))

    call b_ptr%print

    if ((lbound(b1_ptr, 1) /= -1) .or. (ubound(b1_ptr,1) /= 0)) error stop 1_4

    call b1_ptr(-1)%print
    call b1_ptr(0)%print

    if (any(lbound(b2_ptr) /= 0)) error stop 2_4
    if (any(ubound(b2_ptr) /= (/2,1/))) error stop 3_4

    do j = 0, 1
        do i = 0, 2
            call b2_ptr(i,j)%print
        end do
    end do

    deallocate (b_ptr, b1_ptr, b2_ptr)


    !! test using base type as dynamic type
    print *, ''
    print *, 'second test'
    print *, ''

    b_ptr => genData (100)

    b1_ptr => genData (200, lb=0, ub = 2)

    b2_ptr => genData (300, lb = (/-1, 0/), ub = (/0, 0/))

    call b_ptr%print

    if ((lbound(b1_ptr,1) /= 0) .or. (ubound(b1_ptr,1) /= 2)) error stop 4_4

    call b1_ptr(0)%print
    call b1_ptr(1)%print
    call b1_ptr(2)%print

    if (any(lbound (b2_ptr) /= (/-1,0/))) error stop 5_4
    if (any (ubound(b2_ptr) /= 0))  error stop 6_4

    call b2_ptr(-1,0)%print
    call b2_ptr(0,0)%print
end
