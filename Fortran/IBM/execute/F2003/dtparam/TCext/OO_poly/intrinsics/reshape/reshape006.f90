! GB DTP extension using:
! ftcx_dtp -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/intrinsics/reshape/reshape006.f
! with manual adjustment (replace IMPDO in line 79)
! opt variations: -qnol -qdeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: reshape006.f
! %VERIFY: reshape006.out:reshape006.vf
! %STDIN:
! %STDOUT: reshape006.out
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  DATE                       : 11/20/2004
!*  ORIGIN                     :
!*  PRIMARY FUNCTIONS TESTED   : reshape
!*  SECONDARY FUNCTIONS TESTED :
!*  DESCRIPTION                :
!*    SOURCE is non-poly
!*    Assigned data entity is poly
!*    PAD and ORDER are specified. PAD has different declared type but
!*      same dynamic type as SOURCE
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
        integer(k1)   :: i = 88
    end type

    type, extends(Base) :: Child    ! (20,4)
        integer(k1) :: j = 99
    end type
end module

program reshape006
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

    type(Child(20,4)) :: c1(10)
    class(Base(20,4)), pointer :: b1(:,:) => null()
    class(Base(20,4)), allocatable :: b2(:)
    integer i

!   c1 = (/ (Child(20,4)(i,i+1), i=1,10) /)
    do i=1,10
        c1(i) = Child(20,4)(i,i+1)
    end do
    allocate(Child(20,4)::b2(2))

    allocate(b1(3,5))
    b1 = reshape(c1, (/3,5/), b2, (/2,1/))

    print *, c1
    print *, reshape(c1, (/3,5/), b2, (/2,1/))
    print *, b1%i
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
