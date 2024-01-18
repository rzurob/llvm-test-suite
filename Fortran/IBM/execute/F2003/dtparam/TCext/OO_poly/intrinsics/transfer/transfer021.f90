! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=self -qreuse=base /tstdev/OO_poly/intrinsics/transfer/transfer021.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: transfer021.f
! %VERIFY: transfer021.out:transfer021.vf
! %STDIN:
! %STDOUT: transfer021.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 12/16/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : transfer
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SIZE is absent
!*    MOLD is array
!*    SOURCE is array
!*    The result is a rank one array of the same type and type
!*  parameters as MOLD. Its size is as small as possible and its
!*  physical representation is not shorter than that of SOURCE.
!*    Poly and unlimited poly
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
    type Base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (4)
        integer(k1) j(2)
    end type

    type Base1(k2)    ! (4)
        integer, kind  :: k2
        integer(k2)       k
        type(Base(k2)) :: b
    end type
end module

program transfer021
use m
    class(Base1(4)), allocatable :: src1(:,:,:)
    class(*), pointer :: m1(:,:)

    allocate(src1(2,2,2), SOURCE=reshape((/(Base1(4)(i-1,Base(4)(i)),i=1,5)/), &
     (/2,2,2/), (/Base1(4)(-1,Base(4)(-2))/), (/3,2,1/)))
    allocate(Child(4)::m1(2,2))

    select type(name1=>src1)
        type is (Base1(4))
            print *, name1
        class default
            error stop 1_4
    end select

    select type(name1=>transfer(src1, m1))
        type is (Child(4))
            print *, name1(:5), name1(6)%i
        class default
            error stop 2_4
    end select
end
