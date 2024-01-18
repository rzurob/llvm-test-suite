! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/func_return/ffuncRet014a.f
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
!*  DATE                       : 05/12/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : poly-function return (poly-function return is a
!                               global pointer)
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: print => printBase
        procedure, non_overridable :: makeData => updateNGetGPtr
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1)    name

        contains

        procedure :: print => printChild
    end type

    class (base(8)), pointer, private :: gPtr => null()

    contains

    subroutine printGlobal ()
        if (associated(gPtr)) then
            call gPtr%print
        else
            print *, 'pointer not associated'
        end if
    end subroutine

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine updateVal (b, id, name)
        class (base(8)), intent(in), pointer :: b
        integer(8), intent(in) :: id
        character(*), intent(in) :: name

        if (associated (b)) then
            select type (b)
                class is (base(8))
                    b%id = id
                class is (child(8,1,*))
                    b%id = id
                    b%name = name
                class default
                    error stop 10_4
            end select
        end if
    end subroutine

    class (base(8)) function updateNGetGPtr (b)
        pointer updateNGetGPtr
        class (base(8)), intent(in) :: b

        if (associated(gPtr)) deallocate (gPtr)

        allocate (gPtr, source=b)

        updateNGetGPtr => gPtr
    end function
end module

program ffuncRet014a
use m
    class (base(8)), allocatable :: b1

    allocate (b1, source=child(8,1,20)(100_8, 'xlftest team'))

    call updateVal (b1%makeData(), -100_8, 'test xlf')

    call printGlobal

    deallocate (b1)

    allocate (b1, source=base(8)(2000))

    call updateVal (b1%makeData(), 200_8, 'test')

    call printGlobal
end
