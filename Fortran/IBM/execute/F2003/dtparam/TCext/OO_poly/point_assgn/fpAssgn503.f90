! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_poly/point_assgn/fpAssgn503.f
! opt variations: -ql

! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/02/2005
!*
!*  DESCRIPTION                : pointer assignment (sequence type pointer
!                               assigned to unlimited poly target in select type
!                               construct)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program fpAssgn503
    type seq1(k1,k2)    ! (4,8)
        integer, kind :: k1,k2
        sequence

        integer(k1)      i
        real(k2)         r
    end type

    class (*), pointer :: x1(:)

    type (seq1(4,8)), pointer :: s1(:)

    allocate (seq1(4,8):: x1(3))

    select type (y => x1)
        class default
            s1 => y

            call setSeq1(s1)

            write (*, '(3(i5,f10.2))') s1
    end select
end

subroutine setSeq1 (s)
    type seq1(k3,k4)    ! (4,8)
        integer, kind :: k3,k4
        sequence

        integer(k3)      i
        real(k4)         r
    end type

    type(seq1(4,8)), intent(inout) :: s(*)

    s(1:3) = (/(seq1(4,8) (i, 2.0_8*i), i=1, 3)/)
end subroutine
