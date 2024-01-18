! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg011a3.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg011a3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (tests a rank-2 array as
!                               selector in ASSOCIATE construct)
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

    type, extends (base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type
end module

module m1
use m

    contains

    integer(4) function IDSum (b)
        type (base(4)), intent(in) :: b (2,2)

        associate (x => b)
            IDSum = sum (x%id)
        end associate
    end function

    integer(4) function SumID (b)
        class (base(4)), intent(in) :: b(2,3)

        associate (x => b)
            SumID = sum (x%id)
        end associate
    end function
end module

program fArg011a3
use m1
    class (base(4)), allocatable :: b1 (:)

    allocate (b1 (10), source=child(4,1,20) (1, 'b1_array'))

    b1%id = (/(i, i=1,10)/)

    if (IDSum (b1(2:5)) /= 14) error stop 1_4

    if (SumID (b1(2:)) /= 27) error stop 2_4

    deallocate (b1)
end
