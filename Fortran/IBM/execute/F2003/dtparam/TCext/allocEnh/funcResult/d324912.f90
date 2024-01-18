! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/F2003/allocEnh/funcResult/d324912.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 09/06/2006
!*
!*  DESCRIPTION                : miscellaneous (defect 324912)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type base(n1,k1)    ! (20,4)
        integer, kind            :: k1
        integer, len             :: n1
        integer(k1), allocatable :: i
    end type

    type(base(:,4)), allocatable :: i1(:), i2(:), i3(:)

    allocate (base(20,4) :: i1(10), i2(20), i3(20))

    i1 = base(20,4)(10)
    i2 = base(20,4)(100)

    print *, (/(i1(i)%i, i=1,10)/)
    print *, (/(i1(i)%i+i2(i)%i, i=1,10)/)
    i3 = (/(base(20,4)(i1(i)%i+i2(i)%i), i=1,10), i2(11:20)/)

    do i = 1, 20
        print *, i3(i)%i
    end do
    end
