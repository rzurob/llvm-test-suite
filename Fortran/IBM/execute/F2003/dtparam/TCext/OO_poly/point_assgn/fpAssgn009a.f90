! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn009a.f
! opt variations: -qnol -qnodeferredlp

! SCCS ID Information
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: fpAssgn009a.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/09/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : pointer assignment (pointer of sequence types
!*                               or derived type with BIND attributes in the
!*                               assignment with class(*), pointers; use one
!*                               dimensional arraies)
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
    type seq(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        sequence
        integer(k1)      i
    end type

    type, bind(C) :: bType
        integer i
    end type
end module

program fpAssgn009a
use m

    call testSeq

    call testBind

    contains

    subroutine testSeq

        class (*), pointer :: x(:)

        type (seq(20,4)), target :: seq1(-4:5)
        type (seq(:,4)), pointer :: seq_ptr1, seq_ptr2(:)

        seq1 = (/(seq(20,4)(10*i), i=0,9)/)

        x => seq1

        seq_ptr1 => x(3)        ! seq1(3)%i = 70

        seq_ptr2 => x

        if (seq_ptr1%i /= 70) error stop 1_4

        if (size(seq_ptr2) /= 10) error stop 2_4

        if ((lbound(seq_ptr2, 1) /= -4) .or. (ubound(seq_ptr2, 1) /= 5)) &
                    error stop 3_4

        if ((.not. associated (seq_ptr2, x)) .or. (.not. associated &
                    (seq_ptr2, seq1))) error stop 4_4

        do i = 1, 10
            if (seq_ptr2(i-5)%i /= (i-1)*10) error stop 5_4
        end do

        if (.not. associated (seq_ptr1, seq_ptr2(3))) error stop 6_4
    end subroutine

    subroutine testBind
        class(*), pointer :: x(:)

        type (bType), target :: b1(-1:8)
        type (bType), pointer :: b_ptr1, b_ptr2(:)

        b1 = (/(bType (10+i), i=-1,8)/)

        x => b1

        b_ptr2 => x

        b_ptr1 => b1(2)     ! b1(2)%i = 12

        if ((.not. associated (b_ptr2, x)) .or. (.not. associated (b_ptr2, b1))) &
                error stop 10_4

        if (.not. associated (b_ptr1, b_ptr2(2))) error stop 11_4

        if (b_ptr1%i /= 12) error stop 12_4

        if (size(b_ptr2) /= 10) error stop 13_4

        if ((lbound(b_ptr2,1) /= -1) .or. (ubound(b_ptr2,1) /= 8)) error stop 14_4

        do i = 1, 10
            if (b_ptr2(i-2)%i /= 8+i) error stop 15_4
        end do

    end subroutine
end
