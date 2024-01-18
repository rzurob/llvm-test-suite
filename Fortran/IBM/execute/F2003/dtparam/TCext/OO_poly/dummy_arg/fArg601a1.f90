! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg601a1.f
! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fArg601a1.f
! %VERIFY: fArg601a1.out:fArg601a1.vf
! %STDIN:
! %STDOUT: fArg601a1.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 12/06/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited poly
!                               explicit-shape array in sequence association)
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

program fArg601a1
use m
    class (*), pointer :: x1(:)
    integer retVal

    allocate (x1(3), source=(/1_4, 2_4, 3_4/))

    retVal = iType (x1)

    if (retVal /= 3) error stop 1_4

    retVal = iType (x1(::2))

    if (retVal /= 3) error stop 2_4

    deallocate (x1)

    allocate (x1(0:3), source=(/(base(4)(j), j=0,3)/))

    retVal = iType (x1((/3,2,0/)))

    if (retVal /= 1) error stop 3_4

    deallocate (x1)

    allocate (x1(0:2), source=(/child(4,1,15)(0, 'xlftest 00'), child(4,1,15)(1, 'xlftest 01'), &
                        child(4,1,15) (2, 'xlftest 02')/))

    retVal = iType (x1(::2))

    if (retVal /= 2) error stop 4_4

    retVal = iType (x1((/2,2/)))

    if (retVal /= 2) error stop 5_4

    deallocate (x1)
end
