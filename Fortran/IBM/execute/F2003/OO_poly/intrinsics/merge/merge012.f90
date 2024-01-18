! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: merge012.f
! %VERIFY: merge012.out:merge012.vf
! %STDIN:
! %STDOUT: merge012.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE is scalar
!*    FSOURCE is scalar
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

program merge012
use m
    class(*), pointer :: b1
    class(*), allocatable :: b2
    class(*), pointer :: m1(:,:)

    allocate(b1, SOURCE=Child(3,-3))
    allocate(b2, SOURCE=Child(4,-4))
    allocate(m1(3,2), SOURCE=reshape((/.TRUE., .FALSE., &
     .FALSE., .TRUE., .FALSE., .TRUE./),(/3,2/)))

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
        class default
            error stop 2_4
    end select
end
