! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/structureComponent006.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent006.f
! %VERIFY: structureComponent006.out:structureComponent006.vf
! %STDIN:
! %STDOUT: structureComponent006.out
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
!*    is unlimited poly.
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
    type, abstract :: AbstractParent(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    type, extends(AbstractParent) :: Base    ! (4,20)
        integer(k1) i
    end type

    type, extends(Base) :: Child    ! (4,20)
        class(*), pointer :: b2
    end type
end module

program structureComponent006
use m
    class(Base(4,:)), allocatable :: b0(:,:,:)
    type(Child(4,20)) :: c1(4,5)
    type(Child(4,20)) :: c2

    c1%i = reshape((/(i,i=101,120)/), (/4,5/))
    do i=1,4
        do j=1,5
            allocate(c1(i,j)%b2, SOURCE=Base(4,20)(i+j))
        end do
    end do

    c2%i = -1
    allocate(c2%b2, SOURCE=Base(4,20)(-2))

    allocate(b0(3,2,4), SOURCE=reshape(c1, (/3,2,4/), &
     (/c2/), (/3,2,1/)))

    print *, c1%i
    do i=1,5
        do j=1,4
            select type (name1=>c1(j,i)%b2)
                type is (Base(4,*))
                    print *, name1
                class default
                    error stop 1_4
            end select
        end do
    end do

    select type (b0)
        type is (Child(4,*))
            print *, b0%i
            do i=1,4
                do j=1,2
                    do k=1,3
                        select type (name1=>b0(k,j,i)%b2)
                            type is (Base(4,*))
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
