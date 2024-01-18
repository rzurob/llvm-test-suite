! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape020.f
! opt variations: -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape020.f
! %VERIFY: reshape020.out:reshape020.vf
! %STDIN:
! %STDOUT: reshape020.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 11/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is unlimited poly
!*    Assigned data entity is unlimited poly
!*    PAD and ORDER are specified. PAD has different declared type but
!*      same dynamic type as SOURCE
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
        integer(k1)   :: i = 88
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) :: j = 99
    end type
end module

program reshape020
use m
    class(*), pointer :: b1(:) => null()
    class(*), allocatable :: c1(:,:)
    allocate(b1(10), SOURCE = (/ (Child(20,4)(i,i+100), i=1,10) /))

    allocate(c1(3,5), SOURCE = reshape(b1, (/3,5/), &
     (/Child(20,4)(-1,1),Child(20,4)(-2,2)/), (/1,2/)))

    select type (b1)
        type is (Child(*,4))
            print *, b1
        class default
            error stop 1_4
    end select

    select type (c1)
        type is (Child(*,4))
            print *, c1
        class default
            error stop 2_4
    end select
end
