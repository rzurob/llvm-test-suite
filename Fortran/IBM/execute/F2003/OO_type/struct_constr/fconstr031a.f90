!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fconstr031a.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*                                                                     
!*  TEST CASE TITLE            :
!*                                                                     
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 1/13/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (keyword for allocatable
!*                               component, use reshape)
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
    type base
        integer*4, allocatable :: data(:)
        logical*4, allocatable :: mask(:,:)
    end type
end module

program fconstr031a
use m
    logical*4, parameter :: T = .true._4
    logical*4, parameter :: F = .false._4

    integer*4 :: i1(2, 5), ive (10)

    logical*4 :: l1 (10), l2 (3:4, -1:6), lve(2,8)

    type(base) :: b1 = base (null(), null())
    type (base) :: b2 = base (mask=null(), data=null())

    if (allocated(b1%data) .or. allocated(b1%mask) .or. allocated(b2%data) &
        .or. allocated (b2%mask)) error stop 1_4

    forall (i=1:2, j=2:6)  i1(i, j-1) = i+j

    ! what i1 mapped as after this statement
    ive = (/3, 4, 4, 5, 5, 6, 6, 7, 7, 8/)


    forall (i=1:2, j=1:8)  l2(i+2, j-2) = (mod(i+j, 2) == 0)

    !what l2 mapped as after the statement
    lve(1,:) = (/T, F, T, F, T, F, T, F/)
    lve(2,:) = (/F, T, F, T, F, T, F, T/)

    l1 = .true._4

    b1 = base (data = reshape(i1, (/10/)), mask =reshape(l1,(/2,5/)))

    b2 = base (mask = l2, data = reshape (i1(1:1, 2:5), (/4/)))

    if ((size(b1%data) /= 10) .or. (size(b1%mask) /= 10)) error stop 2_4

    do i = 1, 10
        if (b1%data(i) /= ive(i)) error stop 3_4
    end do

    if (.not. all (b1%mask)) error stop 4_4

    if ((size(b2%data) /= 4) .or. (size(b2%mask) /= 16)) error stop 5_4

    if ((lbound(b2%mask, 1) /= 3) .or. (lbound(b2%mask,2) /= -1)) error stop 6_4
    if ((ubound(b2%mask, 1) /= 4) .or. (ubound(b2%mask,2) /= 6)) error stop 7_4

    do i =1, 4
        if (b2%data(i) /= i+3) error stop 8_4
    end do

    do i = 1, 2
        do j = 1, 8
            if (b2%mask(i+2,j-2) .neqv. lve(i,j)) error stop 9_4
        end do
    end do
end
