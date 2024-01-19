! *********************************************************************
!*  ===================================================================
!*  DATE                       : 01/25/2005
!*  PRIMARY FUNCTIONS TESTED   : merge
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    TSOURCE is scalar/array
!*    FSOURCE is scalar/array
!*    MASK is array
!*    Non-poly
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
end module

program merge013
use m
    type(Base) :: b1(3,2)
    type(Base) :: b2
    logical :: m1(3,2)

    b1%i = reshape((/(i,i=1,6)/),(/3,2/))
    b2%i = 4
    m1 = reshape((/.TRUE., .FALSE., .FALSE., .TRUE., &
     .FALSE., .TRUE./),(/3,2/))

    associate(name1=>merge(b1, b2, m1))
        if(.NOT. same_type_as(name1, Base(1))) error stop 1_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate

    associate(name1=>merge(b2, b1, m1))
        if(.NOT. same_type_as(name1, Base(1))) error stop 2_4
        print *, name1
        print *, size(name1)
        print *, shape(name1)
    end associate
end
