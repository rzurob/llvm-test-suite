! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg026a_2.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg026a_2.f
! %VERIFY: fArg026a_2.out:fArg026a_2.vf
! %STDIN:
! %STDOUT: fArg026a_2.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/04/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (implicit interface)
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

    contains

    subroutine printBase (b)
        class (base(4)), intent(in) :: b

        print *, b%id
    end subroutine

    subroutine printChild (b)
        class (child(4,1,*)), intent(in) :: b

        print *, b%id, b%name
    end subroutine
end module

program fArg026a_2
use m
    type (base(4)) :: b1 (10)
    class (base(4)), allocatable :: b2 (:)
    type (child(4,1,20)), target :: c1 (5)

    class (base(4)), pointer :: b3(:)

    b1 = (/(base(4) (i), i=10,1,-1)/)
    c1 = (/(child(4,1,20)(i, 'c1'), i=1,5)/)

    allocate (b2 (5), source=c1)

    allocate (b3 (5))

    b3%id = (/10, 20, 30, 40, 50/)


    call increaseID (b1(2:6))

    call increaseID (b2(1:5))

    call increaseID (b3)

    do i = 1, 5
        call b1(i+1)%print
        call b2(i)%print
        call b3(i)%print
    end do

    call increaseID (b2)

    do i = 1, 5
        call b2(i)%print
    end do

    deallocate (b3, b2)
end

subroutine increaseID (b)
    use m
    type (base(4)), intent(inout) :: b(5)

    do i = 1, 5, 2
        b(i)%id = b(i)%id + 1
    end do
end subroutine
