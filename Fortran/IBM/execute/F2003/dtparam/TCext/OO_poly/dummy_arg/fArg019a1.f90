! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg019a1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg019a1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/27/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (nonelemental procedure
!                               referenced as a defined operator; check for the
!                               rank match)
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
    end type

    interface operator (==)
        logical function cmpArrays (b1, b2)
        import base
            class (base(4)), intent (in) :: b1(:), b2(:)
            dimension cmpArrays (size(b1))
        end function

        logical function cmpArray2scalar (b1, b2)
        import base
            class (base(4)), intent (in) :: b1(:), b2
            dimension cmpArray2scalar (size(b1))
        end function
    end interface
end module

program fArg019a1
use m
    class (base(4)), allocatable :: b1(:)
    type (base(4)) :: b2 (10)
    logical res(10)

    allocate (b1(10))

    b1%id = (/(i, i=1, 10)/)

    b2%id = b1%id

    if (.not. all (b1 .eq. b2)) error stop 1_4

    b2 = b1(3)

    if (.not. all (b2 .eq. b1(3))) error stop 2_4

    res = b1 == b2

    if (.not. res(3)) error stop 3_4

    if (any (res(1:2)) .or. any (res(4:))) error stop 4_4
end

logical function cmpArrays (b1, b2)
use m, only:base
    class (base(4)), intent (in) :: b1(:), b2(:)
    dimension cmpArrays (size(b1))

    cmpArrays = (b1%id == b2%id)
end function

logical function cmpArray2scalar (b1, b2)
use m, only:base
    class (base(4)), intent (in) :: b1(:), b2
    dimension cmpArray2scalar (size(b1))

    cmpArray2scalar = (b1%id == b2%id)
end function
