! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Andy Chen
!*  DATE                       : September 07, 2010
!* .or.GIN                     :
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              :
!*
!*  DESCRIPTION                : Test intrinsic type for array: INTEGER, REAL,
!                                DOUBLE, COMPLEX, CHARACTER, LOGICAL
!*
!*  KEY.or.(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HIS.or.
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

program coindex_assignment_001f
    implicit none
    integer, save    :: caf1(10)[*]
    real(4), save    :: caf2(10,10)[*]
    real(8), save    :: caf3(10)[*]
    complex, save    :: caf4(10)[*]
    logical, save    :: caf6(10)[*]

    integer me, num, left, i, j
    integer    :: array1(10), array2(10)
    real(4)    :: real1(10,10)
    real(4)    :: real2(10,10)
    real(8) d1, d2
    complex    :: cpx2(10)
    character c1
    logical ::logical1(10)
    logical ::logical2(10)

    interface
        logical function precision_r4 (r1, r2)
            real(4), intent(in) :: r1, r2
        end function

        logical function precision_r8 (r1, r2)
            real(8), intent(in) :: r1, r2
        end function

        logical function precision_x8 (r1, r2)
            complex, intent(in) :: r1, r2
        end function
    end interface

    me = THIS_IMAGE()
    num = NUM_IMAGES()

    if (me == 1) then
        left = num
    else
        left = me - 1
    end if

    array1 = [(i*me, i = 1,10)]
    caf1(:)[me] = array1(:)
    forall (i=1:10, j=1:10)
        real1(i,j) = i*j*me
    end forall
    caf2(:,:) = real1(:,:)

    d1 = me*100
    caf3(1) = d1
    caf4 = (/(1.0,1.0*me),(2.0,2.0*me),(3.0,3.0*me),(4.0,4.0*me),(5.0,5.0*me),(6.0,6.0*me),(7.0,7.0*me),(8.0,8.0*me),(9.0,9.0*me),(10.0,10.0*me)/)

    caf6 = [((mod(i,me) .ne. 0), i = 1, 10)]

    SYNC ALL

    logical1(:) = caf6(:)
    logical2(:) = caf6(:)[left]

    d2 = caf3(1)[left]
    array2(:) = caf1(:)[left]
    real2(:,:) = caf2(:,:)[left]
    cpx2(:) = caf4(:)[left]

    do i=1, 10
        if (caf1(i)[me] .ne. array1(i)) then
            error stop 1
        end if
        if (i*left .ne. array2(i)) then
            error stop 2
        end if
    end do

    do i=1, 10 
        do j=1, 10
            if ( .not. co_precision_r4(caf2(i,j), real1(i,j), 1.0e-12)) then
                error stop 3
            end if
            if ( .not. precision_r4(1.0*i*j*left, real2(i,j))) then
                error stop 4
            end if
        end do
    end do

    if (.not. precision_r8(caf3(1)[me], d1)) then
        error stop 5
    end if
    if (.not. precision_r8(100.d0*left,d2)) then
        error stop 6
    end if

    if( ( .not. co_precision_x8( caf4(1), (1.0,1.0*me), 1.0e-12 ) ) .or. &
        ( .not. co_precision_x8( caf4(2), (2.0,2.0*me), 1.0e-12 ) ) .or. &
        ( .not. co_precision_x8( caf4(3), (3.0,3.0*me), 1.0e-12 ) ) .or. &
        ( .not. co_precision_x8( caf4(4), (4.0,4.0*me), 1.0e-12 ) ) .or. &
        ( .not. co_precision_x8( caf4(5), (5.0,5.0*me), 1.0e-12 ) ) .or. &
        ( .not. co_precision_x8( caf4(6), (6.0,6.0*me), 1.0e-12 ) ) .or. &
        ( .not. co_precision_x8( caf4(7), (7.0,7.0*me), 1.0e-12 ) ) .or. &
        ( .not. co_precision_x8( caf4(8), (8.0,8.0*me), 1.0e-12 ) ) .or. &
        ( .not. co_precision_x8( caf4(9), (9.0,9.0*me), 1.0e-12 ) ) .or. &
        ( .not. co_precision_x8( caf4(10), (10.0,10.0*me), 1.0e-12 ) ) )then
        error stop 7
    end if 

    if( ( .not. precision_x8( cpx2(1), (1.0,1.0*left) ) ) .or. &
        ( .not. precision_x8( cpx2(2), (2.0,2.0*left) ) ) .or. &
        ( .not. precision_x8( cpx2(3), (3.0,3.0*left) ) ) .or. &
        ( .not. precision_x8( cpx2(4), (4.0,4.0*left) ) ) .or. &
        ( .not. precision_x8( cpx2(5), (5.0,5.0*left) ) ) .or. &
        ( .not. precision_x8( cpx2(6), (6.0,6.0*left) ) ) .or. &
        ( .not. precision_x8( cpx2(7), (7.0,7.0*left) ) ) .or. &
        ( .not. precision_x8( cpx2(8), (8.0,8.0*left) ) ) .or. &
        ( .not. precision_x8( cpx2(9), (9.0,9.0*left) ) ) .or. &
        ( .not. precision_x8( cpx2(10), (10.0,10.0*left) ) ) )then
        error stop 8
    end if 

    do i=1, 10
        if ( (logical1(i) .neqv. (mod(i,me) .ne. 0)) ) then
            print *,i,",",me,",",logical1(i),",",(mod(i,me) .ne. 0)
            error stop 9
        end if
        if (logical2(i) .neqv. (mod(i,left) .ne. 0)) then
            error stop 10
        end if
    end do

    contains

    logical function co_precision_r8 (x, y, relative)
        real(8) :: x[*], y, relative
        co_precision_r8 = abs(x-y) <= abs(x+y)*relative*0.5d0
    end function

    logical function co_precision_r4 (x, y, relative)
        real(4) :: x[*], y, relative
        co_precision_r4 = abs(x-y) <= abs(x+y)*relative*0.5
    end function

    logical function co_precision_x8 (x, y, relative)
        complex :: x[*], y
        real(4) :: relative
        co_precision_x8 =  &
            (abs(real(x)-real(y)) <= abs(real(x)+real(y))*relative*0.5) .and. &
            (abs(aimag(x)-aimag(y)) <= abs(aimag(x)+aimag(y))*relative*0.5)
    end function

end program
