! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg028a3.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/25/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (sequence association for
!                               rank-two dummy-arg array)
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
        real(k1)      :: pos

        contains

        procedure :: print => printBase
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        write (*, '(f10.2)') b%pos
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print '(f10.2,a,a)', b%pos, ', name=', b%name
    end subroutine

    subroutine print2 (b)
        class (base(4)) b(0:1,2)

        call b(0,1)%print
        call b(0,2)%print
    end subroutine

    subroutine print2X (x)
        class (*) x(2,2)
    end subroutine
end module

program fArg028a3
use m
    class (base(4)), allocatable :: b1(:)

    type (child(4,1,15)) :: c1 (10)

    c1%pos = (/(i*1.0, i=1,10)/)

    c1%name = 'xlftest 101'

    call print2 (c1(3))

    allocate (b1(0:9), source=c1)

    call print2 (b1((/5,6,7,8,7/)))   !<-- same as call print2(c1(6))
end