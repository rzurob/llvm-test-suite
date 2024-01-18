! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/intrinsics/reshape/structureComponent005.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent005.f
! %VERIFY: structureComponent005.out:structureComponent005.vf
! %STDIN:
! %STDOUT: structureComponent005.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 12/06/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                : SOURCE is a structure component, which
!*    is a scalar. The object containing the component is an array and
!*    is poly.
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
    type, abstract :: AbstractParent(k1)    ! (4)
        integer, kind :: k1
    end type

    type, extends(AbstractParent) :: Base    ! (4)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4)
        class(Base(k1)), pointer :: b2
    end type
end module

program structureComponent005
use m
    class(Base(4)), allocatable :: b0(:,:,:)
    type(Child(4)) :: c1(4,5)
    type(Child(4)) :: c2

    c1%i = reshape((/(i,i=101,120)/), (/4,5/))
    do i=1,4
        do j=1,5
            allocate(c1(i,j)%b2, SOURCE=Base(4)(i+j))
        end do
    end do

    c2%i = -1
    allocate(c2%b2, SOURCE=Base(4)(-2))

    allocate(b0(3,2,4), SOURCE=reshape(c1, (/3,2,4/), &
     (/c2/), (/3,2,1/)))

    print *, c1%i
    do i=1,5
        do j=1,4
            select type (name1=>c1(j,i)%b2)
                type is (Base(4))
                    print *, name1
                class default
                    error stop 1_4
            end select
        end do
    end do

    select type (b0)
        type is (Child(4))
            print *, b0%i
            do i=1,4
                do j=1,2
                    do k=1,3
                        select type (name1=>b0(k,j,i)%b2)
                            type is (Base(4))
                                print *, name1
                            class default
                                error stop 2_4
                        end select
                    end do
                end do
            end do
        class default
            error stop 3_4
    end select
end
