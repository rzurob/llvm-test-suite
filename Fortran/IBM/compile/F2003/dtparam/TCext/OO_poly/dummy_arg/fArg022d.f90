! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg022d.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp fArg022d.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 08/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (array section with vector
!                               subscript is NOT definable; and shall not be
!                               used as the actual argument to be associated
!                               with a dummy argument with INTENT(OUT))
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

        procedure, non_overridable :: assgnId => assignID2Base
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    elemental subroutine assignID2Base (b, id)
        class (base(4)), intent(inout) :: b
        integer*4, intent(in) :: id

        b%id = id
    end subroutine
end module

program fArg022d
use m
    interface assignment(=)
        subroutine base2Base (b1, b2)
        use m
            type (base(4)), intent(out) :: b1(:)
            class (base(4)), intent(in) :: b2(:)
        end subroutine
    end interface

    class (base(4)), allocatable :: b1(:)

    type (child(4,1,20)) :: c1(4)
    integer vec(4)

    allocate (b1(size(c1)), source=(/child(4,1,20)(1, 'b1_1'), child(4,1,20)(2, 'b1_2'), &
                child(4,1,20)(3,'b1_3'), child(4,1,20)(4,'b1_4')/))


    !

    c1 = (/child(4,1,20)(40, 'c1_static_1'), child(4,1,20)(30, 'c1_static_2'), &
                    child(4,1,20)(20,'c1_static_3'), child(4,1,20)(10, 'c1_static_1')/)

    vec = (/4,3,2,1/)

    b1 (vec) = c1     !<-- illegal
end

subroutine base2Base (b1, b2)
use m
    type (base(4)), intent(out) :: b1(:)
    class (base(4)), intent(in) :: b2(:)

    call b1%assgnID(b2%id)
end subroutine
