! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodeferredlp /tstdev/F2003/misc/d318884.f
! opt variations: -qck -qnok -qnol -qdeferredlp

!defect 318884
module m
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        character(:), allocatable :: names
    end type
end module

use m
    type(base(4,20)), allocatable :: b1(:)

    character(:), pointer :: c1

    allocate(b1(5))

    allocate(c1, source='where to start?')

    b1(1) = base(4,20)('abcd')
    b1(2) = base(4,20)('xlf'//'test')
    b1(3) = base(4,20)(c1)

    b1(4) = b1(2)

    b1(5) = base(4,20)(b1(2)%names//' '//b1(3)%names)

    do i = 1, 5
        print *, b1(i)%names
    end do
end
