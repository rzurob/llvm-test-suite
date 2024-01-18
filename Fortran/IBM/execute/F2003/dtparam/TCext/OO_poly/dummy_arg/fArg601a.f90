! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg601a.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg601a.f
! %VERIFY: fArg601a.out:fArg601a.vf
! %STDIN:
! %STDOUT: fArg601a.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/03/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly
!                               explicit-shape array dummy-arg; sequence
!                               association)
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

    type, extends(base) :: child(k2,n1)    ! (4,1,15)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    contains

    function iType (x)
        class (*), intent(in) :: x (2)

        select type (x)
            type is (base(4))
                print *, x
                iType = 1
            type is (child(4,1,*))
                print *, x
                iType = 2
            type is (integer(4))
                print *, x
                iType = 3
            class default
                print *, 'other type'
                iType = -1
        end select
    end function
end module

program fArg601a
use m

    type (child(4,1,15)), target :: c1(5)
    class (base(4)), pointer :: b2(:)

    integer(4) :: i(10), retVal

    i = 10

    c1%id = (/1,2,3,4,5/)
    c1%name = (/'c1_1', 'c1_2', 'c1_3', 'c1_4', 'c1_5'/)

    allocate (b2(0:3), source=(/(child(4,1,15)(j, 'xlftest 101'), j=0,3)/))

    retVal = iType (i)

    if (retVal /= 3) error stop 1_4

    retVal = iType (b2)

    if (retVal /= 2) error stop 2_4

    retVal = iType (b2(1:3:2))

    if (retVal /= 2) error stop 3_4

    retVal = iType (c1)

    if (retVal /= 2) error stop 4_4

    deallocate (b2)

    b2 => c1(2::3)

    retVal = iType (b2)

    if (retVal /= 2) error stop 5_4
end
