! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet013.f
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
!*  DATE                       : 05/10/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly-function return (intented for abstract
!                               type testing; actually becomes testing
!                               recursive)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type, abstract :: base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type
end module

module m1
use m
    type, extends(base) :: child(k2)    ! (4,20,8)
        integer, kind              :: k2
        class(base(k1,:)), pointer :: next => null()
        integer(k2)                   id

        contains

        final :: finalizeChild
        procedure :: print => printChild
    end type

    interface assignment(=)
        module procedure assgnB1B2
    end interface

    contains

    class(child(4,:,8)) function makeNode (id)
        pointer makeNode
        integer(8), intent(in) :: id

        allocate (makeNode, source=child(4,20,8)(null(), id))
    end function

    recursive subroutine finalizeChild (c)
        type (child(4,*,8)), intent(inout) :: c

        if (associated(c%next)) then
            print *, 'deallocating nodes'

            deallocate (c%next)
        end if
    end subroutine

    recursive subroutine printChild (c)
        class (child(4,*,8)), intent(in) :: c

        print *, c%id

        if (associated(c%next)) then
            select type (x => c%next)
                class is (child(4,*,8))
                    call x%print
                class default
                    error stop 15_4
            end select
        end if
    end subroutine

    recursive subroutine assgnB1B2 (b1, b2)
        class (child(4,*,8)), intent(out) :: b1
        class (child(4,*,8)), intent(in) :: b2

        b1%id = b2%id

        if (associated(b2%next)) then
            allocate (child(4,20,8):: b1%next)

            select type (x1 => b1%next)
                type is (child(4,*,8))
                    select type (x2 => b2%next)
                        type is (child(4,*,8))
                            x1 = x2
                        class default
                            error stop 11_4
                    end select
                class default
                    error stop 10_4
            end select
        end if
    end subroutine
end module

program ffuncRet013
use m1
    type(child(4,20,8)) c1, c2

    c1 = child(4,20,8)(makeNode(10_8), 1_8)

    print *, 'test 2'

    select type (x => c1%next)
        class is (child(4,*,8))
            x%next => makeNode(100_8)
        class default
            error stop 2_4
    end select

    call c1%print

    c2 = c1
    c2 = c1

    if (associated (c2%next, c1%next)) error stop 1_4

    call c2%print

    print *, 'end'
end
