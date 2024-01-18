! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc021a.f
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
! %GROUP: falloc021a.f
! %VERIFY: falloc021a.out:falloc021a.vf
! %STDIN:
! %STDOUT: falloc021a.out
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
!*  DESCRIPTION                : ALLOCATE (for pointer function result, it can
!                               be disassociated)
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
    type base(k1)    ! (2)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
        procedure, nopass :: printType => printBaseType
    end type

    type, extends(base) :: child(k2,n1)    ! (2,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
        procedure, nopass :: printType => printChildType
    end type

    interface genData
        class (base(2)) function genBasePtr (id)
        import base
            integer(4), intent(in), optional :: id
            pointer genBasePtr
        end function

        class (child(2,1,15)) function genChildPtr (id, name)
        import child
            integer(4), intent(in), optional :: id
            character (*), intent(in) :: name
            pointer genChildPtr
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(2)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printBaseType
        print *, 'base'
    end subroutine

    subroutine printChild (b)
        class (child(2,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printChildType
        print *, 'child'
    end subroutine
end module


class (base(2)) function genBasePtr (id)
use m, only: base
    integer(4), intent(in), optional :: id
    pointer genBasePtr

    if (present (id)) then
        allocate (genBasePtr, source=base(2)(id))
    else
        genBasePtr => null()
    end if
end function


class (child(2,1,15)) function genChildPtr (id, name)
use m, only : child
    integer(4), intent(in), optional :: id
    character(*), intent(in) :: name
    pointer genChildPtr

    if (present (id)) then
        allocate (genChildPtr, source = child(2,1,15)(id, name))
    else
        nullify (genChildPtr)
    end if
end function


program falloc021a
use m
    class (base(2)), pointer :: b1_ptr

    b1_ptr => genData (10)

    call b1_ptr%print

    deallocate (b1_ptr)

    b1_ptr => genData ()

    if (associated (b1_ptr)) error stop 1_4

    b1_ptr => genData (1, 'test1')

    call b1_ptr%print

    deallocate (b1_ptr)

    b1_ptr => genData (name = 'abc')

    if (associated (b1_ptr)) error stop 2_4

    call b1_ptr%printType
end
