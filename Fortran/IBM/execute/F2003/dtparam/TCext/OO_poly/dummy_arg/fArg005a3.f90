! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005a3.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg005a3.f
! %VERIFY: fArg005a3.out:fArg005a3.vf
! %STDIN:
! %STDOUT: fArg005a3.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/05/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (poly-pointer dummy-arg's
!*                               association; scalars and arrays)
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

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name

        contains

        procedure :: print => printChild
    end type

    interface
        subroutine createBase (b, id, name)
        import base
            class (base(4)), pointer, intent(out) :: b
            integer*4, intent(in) :: id
            character(*), intent(in), optional :: name
        end subroutine

        subroutine createBaseArray (b, id, name, arraySize)
        import base
            class (base(4)), pointer, intent(out) :: b(:)
            integer*4, intent(in) :: id, arraySize
            character(*), intent(in), optional :: name
        end subroutine
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
end module

program fArg005a3
use m
    class (base(4)), pointer :: b1, b2(:)

    type (child(4,1,20)), target :: c1, c2(2:4)

    b1 => c1
    b2 => c2

    call createBase (b1, 10, name='b1_pointer')

    call b1%print

    deallocate (b1)

    call createBase (b1, 15)

    call b1%print

    call createBaseArray (b2, 20, arraySize=2)

    if (size(b2) /= 2) error stop 1_4

    call b2(1)%print
    call b2(2)%print

    deallocate (b2)

    call createBaseArray (b2, 25, 'b2_array_of_3', 3)

    if (size(b2) /= 3) error stop 2_4

    call b2(1)%print
    call b2(2)%print
    call b2(3)%print

    deallocate (b1, b2)
end


subroutine createBase (b, id, name)
use m, only : base, child
    class (base(4)), pointer, intent(out) :: b
    integer*4, intent(in) :: id
    character(*), intent(in), optional :: name

    if (present (name)) then
        allocate (b, source=child(4,1,20)(id, name))
    else
        allocate (b)
        b%id = id
    end if
end subroutine


subroutine createBaseArray (b, id, name, arraySize)
use m, only : base, child
    class (base(4)), pointer, intent(out) :: b(:)
    integer*4, intent(in) :: id, arraySize
    character(*), intent(in), optional :: name

    if (present (name)) then
        allocate (b(arraySize), source=child(4,1,20)(id, name))
    else
        allocate (b(arraySize))
        b%id = id
    end if
end subroutine
