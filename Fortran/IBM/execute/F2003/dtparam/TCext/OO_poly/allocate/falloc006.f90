! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc006.f
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
! %GROUP: falloc006.f
! %VERIFY: falloc006.out:falloc006.vf
! %STDIN:
! %STDOUT: falloc006.out
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
!*  DESCRIPTION                : allocate (unlimited poly allocatable scalars in
!                               allocate stmt with type-spec)
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
    type base(k1)    ! (8)
        integer, kind :: k1
        integer(k1)      id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (8,1,15)
        integer, kind                      :: k2
        integer, len                       :: n1
        character(kind=k2,len=n1), pointer :: name => null()

        contains

        procedure :: print => printChild
        final :: finalizeChild, finalizeChildRank1
    end type

    contains

    subroutine printBase (b)
        class (base(8)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(8,1,*)), intent(in) :: b

        if (associated (b%name)) then
            print *, b%id, b%name
        else
            print *, b%id, 'null'
        end if
    end subroutine

    subroutine finalizeChild (c)
        type (child(8,1,*)), intent(inout) :: c
        integer err

        print *, 'finalizeChild'

        if (associated (c%name)) then
            print *, 'deallocating name'
            deallocate (c%name, stat=err)

            if (err /= 0) print *, 'deallocating data failed'
        end if
    end subroutine

    subroutine finalizeChildRank1 (c)
        type (child(8,1,*)), intent(inout) :: c(:)

        print *, 'finalizeChildRank1'

        do i = 1, size(c)
            call finalizeChild (c(i))
        end do
    end subroutine
end module

program falloc006
use m
    class (*), pointer :: x0, x1(:)

    allocate (child(8,1,15):: x0)

    select type(x0)
        type is (base(8))
            x0%id = 120

        type is (child(8,1,*))
            x0%id = 220
            allocate (x0%name, source='xlftest abc')
    end select

    select type (x0)
        class is (base(8))
            call x0%print
    end select

    deallocate (x0)
end
