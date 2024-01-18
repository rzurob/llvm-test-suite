! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE or FSOURCE is function return of transfer.
!*    Poly and unlimited poly.
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
        integer j
    end type
end module

program transfer001
use m
    class(AbstractParent), pointer :: c1(:,:,:)
    class(AbstractParent), allocatable :: b1(:)
    class(*), pointer :: b2
    class(*), pointer :: c2(:)

    allocate(c1(2,2,2), SOURCE=reshape((/(Child(i,i-1),i=101,108)/), &
     (/2,2,2/)))
    allocate(b2, SOURCE=Base(-2))
    allocate(c2(6), SOURCE=(/(Base(i),i=1,6)/))

    select type(name1=>merge(transfer(c1, b2), c2, .TRUE.))
        type is (Base)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 1_4
    end select

    allocate(b1(8), SOURCE=(/(Child(i,-i),i=1,8)/))

    select type(name1=>merge(transfer(b2,b1,3),transfer(c2,c1),.FALSE.))
        type is (Child)
            print *, name1
            print *, size(name1)
            print *, shape(name1)
        class default
            error stop 2_4
    end select
end
