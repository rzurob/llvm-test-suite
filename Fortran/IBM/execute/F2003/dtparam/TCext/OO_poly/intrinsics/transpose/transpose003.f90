! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_poly/intrinsics/transpose/transpose003.f
! opt variations: -qnol -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transpose003.f
! %VERIFY: transpose003.out:transpose003.vf
! %STDIN:
! %STDOUT: transpose003.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/30/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transpose
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : MATRIX is unlimited poly
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

    type, extends(Base) :: Child(n2,k2)    ! (20,4,20,4)
        integer, kind :: k2
        integer, len  :: n2
        integer(k2)      j
    end type
end module

program transpose003
use m
    class(*), allocatable :: b1(:,:)
    class(*), pointer :: b2(:,:)

    allocate(b1(2,4), SOURCE=reshape((/(Base(20,4)(i),i=1,8)/), (/2,4/)))
    allocate(b2(3,3), SOURCE=reshape((/(Child(20,4,20,4)(i,i-1),i=11,19)/), &
     (/3,3/)))

    select type(name1=>transpose(b1))
        type is (Base(*,4))
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>transpose(b2))
        type is (Child(*,4,*,4))
            print *, name1
        class default
            error stop 2_4
    end select
end
