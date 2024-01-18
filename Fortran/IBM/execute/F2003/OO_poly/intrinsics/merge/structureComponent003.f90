! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: structureComponent003.f
! %VERIFY: structureComponent003.out:structureComponent003.vf
! %STDIN:
! %STDOUT: structureComponent003.out
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
!*    TSOURCE or FSOURCE is a structure component, which is a scalar.
!*  The object containing the component is an array and is poly.
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
    type, abstract :: AbstractParent
    end type

    type, extends(AbstractParent) :: Base
        integer i
    end type

    type, extends(Base) :: Child
        type(Base) :: b1
    end type
end module

program structureComponent003
use m
    class(AbstractParent), allocatable :: c1(:,:,:)
    class(*), pointer :: b2(:,:,:)
    logical :: m1(2,2,2)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(i,Base(-i)), &
     i=101,108)/), (/2,2,2/)))
    allocate(b2(2,2,2), SOURCE=reshape((/(Base(-i),i=1,8)/),(/2,2,2/)))
    m1 = reshape((/ .TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE., &
     .TRUE.,.FALSE. /), (/2,2,2/))

    select type(c1)
        type is (Child)
            associate(name1=>merge(c1%b1, b2, m1))
                if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
                print *, name1
                print *, size(name1)
                print *, shape(name1)
            end associate
        class default
            error stop 2_4
    end select
end
