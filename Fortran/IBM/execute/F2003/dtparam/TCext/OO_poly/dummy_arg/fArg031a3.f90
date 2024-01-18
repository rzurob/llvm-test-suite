! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg031a3.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg031a3.f
! %VERIFY: fArg031a3.out:fArg031a3.vf
! %STDIN:
! %STDOUT: fArg031a3.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/14/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (dummy_arg used as
!                               actual-arg; test squence association)
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
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    private internalT

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine

    subroutine test1 (c)
        class (child(4,1,*)), intent(in) :: c(3)

        call internalT (c)
    end subroutine

    subroutine test2 (c)
        type (child(4,1,*)), intent(in) :: c(3)

        call internalT (c)
    end subroutine

    subroutine internalT (b)
        class (base(4)), intent(in) :: b(3)

        call b(1)%print
        call b(2)%print
        call b(3)%print
    end subroutine
end module

program fArg031a3
use m
    type (child(4,1,20)) :: c1 (3)

    c1 = (/child(4,1,20)(1,'c1_1'), child(4,1,20)(2,'c1_2'), child(4,1,20)(3,'c1_3')/)


    call test1 (c1)

    call test1 ((/child(4,1,20)(10, 'temp1'), child(4,1,20)(20, 'temp2'), child(4,1,20)(30, 'temp3')/))

    call test2 (c1)

    call test2 ((/child(4,1,20)(10, 'temp1'), child(4,1,20)(20, 'temp2'), child(4,1,20)(30, 'temp3')/))
end
