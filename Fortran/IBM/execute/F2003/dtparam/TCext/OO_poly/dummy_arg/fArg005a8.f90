! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=self /tstdev/OO_poly/dummy_arg/fArg005a8.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/28/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : argument association (unlimited
!                               poly-allocatable dummy-arg array to be
!                               associated only with the unlimited
!                               poly-allocatable actual-arg array)
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    contains

    subroutine copyData (x, x1)
        class (*), allocatable, intent(out) :: x(:)
        class (*), intent(in) :: x1(:)

        allocate (x(1:size(x1)), source=x1)
    end subroutine
end module

program fArg005a8
use m
use ISO_C_BINDING

    type seq1(n1,k1,k2)    ! (20,4,2)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence
        integer(k1)      i1
        integer(k2)      i2
        integer(k1)      i3
    end type

    type, BIND(C) :: bType
        integer(c_short) i1
        integer(c_int) i2
    end type

    type (seq1(:,4,2)), pointer :: s1(:)
    type (bType), pointer :: b1(:)

    class (*), allocatable, target :: x1(:), x2(:)

    type (seq1(20,4,2)) :: s2 (3)
    type (bType) :: b2 (2:5)

    s2 = (/(seq1(20,4,2)(i,i+1,2*i+1), i=1,3)/)

    b2 = (/(bType (i, 2*i),i=2,5)/)

    call copyData (x1, s2)

    call copyData (x2, b2)

    s1 => x1
    b1 => x2

    if ((size(s1) /= 3) .or. (size(b1) /= 4)) error stop 1_4

    do i = 1,3
        if ((s1(i)%i1 /= i) .or. (s1(i)%i2 /= i+1) .or. &
            (s1(i)%i3 /= 2*i+1))    error stop 2_4
    end do

    do i = 2, 5
        if ((b1(i-1)%i1 /= i) .or. (b1(i-1)%i2 /= 2*i)) error stop 3_4
    end do
end
