! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape004.f
! with manual adjustment (replace IMPDO line 77)
! opt variations: -qnol -qdeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is poly
!*    PAD and ORDER are not specified
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
    type Base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      i
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) j
    end type
end module

program reshape004
use m

    interface assignment (=)
        elemental subroutine tt(c, t)
            use m
            type(Child(*,4)), intent(out) :: c
            type(Child(*,4)), intent(in) :: t
        end subroutine

        elemental subroutine ct(c, t)
            use m
            type(Base(*,4)), intent(out) :: c
            type(Child(*,4)), intent(in) :: t
        end subroutine
    end interface

    type(Child(20,4)) :: c1(20)
    class(Base(20,4)), pointer :: b2(:,:) => null()
    integer i

!   c1 = (/ (Child(20,4)(i,i+1), i=1,20) /)
	do i=1,20
        c1(i) = Child(20,4)(i,i+1)
    end do

    allocate(b2(3,5))
    b2 = reshape(c1, (/3,5/))

    print *, c1
    print *, b2%i
end

elemental subroutine tt(c, t)
    use m
    type(Child(*,4)), intent(out) :: c
    type(Child(*,4)), intent(in) :: t
    c%i = t%i
    c%j = t%j
end subroutine

elemental subroutine ct(c, t)
    use m
    type(Base(*,4)), intent(out) :: c
    type(Child(*,4)), intent(in) :: t
    c%i = t%i
end subroutine
