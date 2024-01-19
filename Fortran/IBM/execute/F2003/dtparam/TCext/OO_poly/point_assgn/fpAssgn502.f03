! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp /tstdev/OO_poly/point_assgn/fpAssgn502.f
! opt variations: -qnol -qnodeferredlp

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/02/2005
!*
!*  DESCRIPTION                : pointer assignment (sequence type pointer
!                               assigned to unlimited poly target in associate
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

program fpAssgn502
    interface
        subroutine setSeq1 (s)
            type seq1(n1,k1,k2)    ! (20,4,8)
                integer, kind :: k1,k2
                integer, len  :: n1
                sequence

                integer(k1)      i
                real(k2)         r
            end type

            type(seq1(20,4,8)), intent(inout) :: s(*)
        end subroutine
    end interface

    type seq1(n1,k1,k2)    ! (20,4,8)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence

        integer(k1)      i
        real(k2)         r
    end type

    class (*), pointer :: x1(:)

    type (seq1(20,4,8)), pointer :: s1(:)

    allocate (seq1(20,4,8):: x1(3))

    associate (y => x1)
        s1 => y

        call setSeq1(s1)

        write (*, '(3(i5,f10.2))') s1
    end associate
end

subroutine setSeq1 (s)
    type seq1(n1,k1,k2)    ! (20,4,8)
        integer, kind :: k1,k2
        integer, len  :: n1
        sequence

        integer(k1)      i
        real(k2)         r
    end type

    type(seq1(20,4,8)), intent(inout) :: s(*)

    s(1:3) = (/(seq1(20,4,8) (i, 1.5_8*i), i=1, 3)/)
end subroutine
