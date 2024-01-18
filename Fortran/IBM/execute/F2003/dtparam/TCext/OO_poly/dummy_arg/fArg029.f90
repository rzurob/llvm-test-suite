! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg029.f
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
! %GROUP: fArg029.f
! %VERIFY: fArg029.out:fArg029.vf
! %STDIN:
! %STDOUT: fArg029.out
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
!*  DATE                       : 06/09/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (function return results
!                               as actual-arg)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name ='default'

        contains

        procedure :: print => printChild
    end type

    interface makeData
        function makeBase (id, n)
        import base
            integer*4, intent(in) :: id, n
            type (base(4)) :: makeBase(n)
        end function

        function makeChildData (id, name, n)
        import child
            integer*4, intent(in) :: id, n
            character(*), intent(in) :: name

            type (child(4,1,20)) :: makeChildData (n)
        end function
    end interface

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printData (b)
        class (base(4)), intent(in) :: b(:)

        do i = 1, size (b)
            call b(i)%print
        end do
    end subroutine
end module

program fArg029
use m
    call printData (makeData (10, 3))

    call printData (makeData (20, 'temps', 2))
end

type (base(4)) function makeBase (id, n)
use m, only: base
    integer*4, intent(in) :: id, n
    dimension makeBase(n)

    makeBase%id = id
end function

type (child(4,1,20)) function makeChildData (id, name, n)
use m, only:child
    integer*4, intent(in) :: id, n
    character(*), intent(in) :: name
    dimension makeChildData (n)

    makeChildData%id = id
    makeChildData%name = name
end function
