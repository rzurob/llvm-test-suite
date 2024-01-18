! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: merge015.f
! %VERIFY: merge015.out:merge015.vf
! %STDIN:
! %STDOUT: merge015.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  TEST CASE TITLE            :
!*  PROGRAMMER                 : Yong Du
!*  DATE                       : 01/25/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DRIVER STANZA              : xlf90
!*  DESCRIPTION                :
!*    TSOURCE is scalar/array
!*    FSOURCE is scalar/array
!*    MASK is array
!*    Unlimited-poly
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
    type Base
        integer :: i = 8
    end type

    type, extends(Base) :: Child
        integer :: j = 9
    end type
end module

program merge015
use m
    class(*), pointer :: b1(:,:)
    class(*), allocatable :: b2
    class(*), pointer :: m1(:,:)

    allocate(b1(3,2), SOURCE=reshape((/(Child(i,-i),i=1,6)/),(/3,2/)))
    allocate(b2, SOURCE=Child(7,8))
    allocate(m1(3,2), SOURCE=reshape((/.TRUE., .FALSE., .FALSE., &
     .TRUE., .FALSE., .TRUE./),(/3,2/)))

    select type(m1)
        type is (logical)
            select type(name1=>merge(b1, b2, m1))
                type is (Child)
                    print *, name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 1_4
            end select

            select type(name1=>merge(b2, b1, m1))
                type is (Child)
                    print *, name1
                    print *, size(name1)
                    print *, shape(name1)
                class default
                    error stop 2_4
            end select
        class default
            error stop 3_4
    end select
end
