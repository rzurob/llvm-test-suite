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
! %GROUP: ffuncRet501.f
! %VERIFY: ffuncRet501.out:ffuncRet501.vf
! %STDIN:
! %STDOUT: ffuncRet501.out
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
!*  DATE                       : 12/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : poly function return (poly function results in
!                               reshape() and used as the actual arg)
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
    type base
        integer(selected_int_kind (2)) :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child
        character(15) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    class (base) function produceBaseAlloc (id, name, nsize)
        allocatable produceBaseAlloc(:)
        integer(selected_int_kind(2)), intent(in) :: id
        integer(4), intent(in) :: nsize
        character(*), optional, intent(in) :: name

        if (present(name)) then     ! allocate of type child
            allocate (produceBaseAlloc(nsize), &
                    source=(/(child(i,name), i=ID, id+nsize)/))
        else
            allocate (produceBaseAlloc(nsize), &
                    source=(/(base(i), i=id, id+nsize)/))
        end if
    end function

    subroutine printData (d)
        class (base), intent(in) :: d (:,:)

        do j = 1, size(d,2)
            do i = 1, size(d,1)
                call d(i,j)%print
            end do
        end do
    end subroutine
end module


program ffuncRet501
use m
    call printData (reshape (produceBaseAlloc (1_1, 'test', 4), (/2,2/)))
end
