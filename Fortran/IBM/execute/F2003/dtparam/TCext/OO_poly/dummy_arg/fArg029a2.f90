! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg029a2.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg029a2.f
! %VERIFY: fArg029a2.out:fArg029a2.vf
! %STDIN:
! %STDOUT: fArg029a2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
        function makeBase (id)
        import base
            integer*4, intent(in) :: id
            type (base(4)) :: makeBase
        end function

        function makeChildData (id, name)
        import child
            integer*4, intent(in) :: id
            character(*), intent(in) :: name

            type (child(4,1,20)) :: makeChildData
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
        class (base(4)), intent(in) :: b

        call b%print
    end subroutine
end module

program fArg029a2
use m
    call printData (makeData (10))

    call printData (makeData (20, 'temps'))
end

type (base(4)) function makeBase (id)
use m, only: base
    integer*4, intent(in) :: id

    makeBase%id = id
end function

type (child(4,1,20)) function makeChildData (id, name)
use m, only:child
    integer*4, intent(in) :: id
    character(*), intent(in) :: name

    makeChildData%id = id
    makeChildData%name = name
end function
