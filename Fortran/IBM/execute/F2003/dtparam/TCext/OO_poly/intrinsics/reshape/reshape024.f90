! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape024.f
! opt variations: -qnol -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape024.f
! %VERIFY: reshape024.out:reshape024.vf
! %STDIN:
! %STDOUT: reshape024.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/05/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is unlimited poly
!*    PAD and ORDER are specified. PAD has same declared type as SOURCE
!*    SOURCE is rank four
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
end module

program reshape024
use m
    type(Child(20,4)) :: c1(10)
    type(Child(20,4)) :: c2(2,3,2,2)
    class(*), allocatable :: b2(:,:)

    c1 = (/ (Child(20,4)(i,i+100), i=1,10) /)
    c2 = reshape(c1, (/2,3,2,2/), (/Child(20,4)(-1,1),Child(20,4)(-2,2)/), (/4,3,1,2/))

    allocate(b2(3,5), SOURCE = reshape(c2, (/3,5/), &
     (/Child(20,4)(-3,3),Child(20,4)(-4,4)/),(/2,1/)))

    print *, c1
    print *, c2

    select type (b2)
        type is (Child(*,4))
            print *, b2
        class default
            error stop 1_4
    end select
end
