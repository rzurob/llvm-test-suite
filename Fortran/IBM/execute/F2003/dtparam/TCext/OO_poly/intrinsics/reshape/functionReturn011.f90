! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/functionReturn011.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: functionReturn011.f
! %VERIFY: functionReturn011.out:functionReturn011.vf
! %STDIN:
! %STDOUT: functionReturn011.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/11/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : SOURCE is the return value of an
!*    external function call. Test the SAVE attribute.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  ===================================================================
!*  REVISION HISTORY
!*                    MM/DD/YY :
!*                        Init :
!*                    Comments :
!*  ===================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901

module m
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type

    contains

    function func1()
        class(Base(:,4)), allocatable, SAVE :: temp(:)
        class(Base(:,4)), allocatable :: func1(:)
        if(.NOT. allocated(temp)) then
            print *, "create Base"
            allocate(temp(2), SOURCE=(/ (Base(20,4)(i), i=1,2) /))
            allocate(func1(20), SOURCE=(/ (Base(20,4)(i), i=1,20) /))
        else
            print *, "create Child"
            deallocate(temp)
            allocate(func1(20), SOURCE=(/ (Child(20,4)(i,i-1), i=1,20) /))
        end if
    end function
end module

program functionReturn011
use m
    class(Base(:,4)), pointer :: b1(:,:) => null()

    allocate(b1(3,5), SOURCE=reshape(func1(), (/3,5/)))

    select type (b1)
        type is (Base(*,4))
            print *, b1
        class default
            error stop 1_4
    end select

    deallocate(b1)
    allocate(b1(5,4), SOURCE=reshape(func1(), (/5,4/)))

    select type (b1)
        type is (Child(*,4))
            print *, b1
        class default
            error stop 2_4
    end select
end
