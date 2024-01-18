! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg010a3.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg010a3.f
! %VERIFY: fArg010a3.out:fArg010a3.vf
! %STDIN:
! %STDOUT: fArg010a3.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/29/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (VALUE attribute; test
!                               non-pointer component)
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
        integer(k1)   :: id = -1

        contains

        procedure :: assignID => assignID2Base
        procedure :: print => printBase
    end type

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'

        contains

        procedure :: print => printChild
    end type

    contains

    !! this subroutine will be reseting the value of other component in the
    !extended types
    subroutine assignID2Base (b, id)
        class (base(4)), intent(out) :: b
        integer*4, intent(in) :: id

        b%id = id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine
end module

program fArg010a3
use m
    type (child(4,1,20)) :: c1 = child(4,1,20) (1, 'c1')

    if (resetTemp (c1) /= 1) error stop 1_4

    call c1%print

    contains

    integer(4) function resetTemp (b)
        class (base(4)) :: b

        resetTemp = b%id

        select type (b)
            class is (child(4,1,*))
                call resetIDChild (b)

            class default
                call resetIDBase (b)

        end select
    end function

    subroutine resetIDBase (b)
        type(base(4)), value :: b

        call b%assignID (100)

        if (b%id /= 100) error stop 10_4

        call b%print
    end subroutine

    subroutine resetIDChild (b)
        type(child(4,1,20)), value :: b

        call b%assignID (100)

        if (b%id /= 100) error stop 20_4

        call b%print
    end subroutine
end
