!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Jim Xia
!*  DATE                       : 02/02/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
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
    type seq1
        sequence

        integer(4) i
        real(8) r
    end type

    class (*), pointer :: x1(:)

    type (seq1), pointer :: s1(:)

    allocate (seq1:: x1(3))

    associate (y => x1)
        s1 => y

        call setSeq1(s1)

        write (*, '(3(i5,f10.2))') s1
    end associate
end

subroutine setSeq1 (s)
    type seq1
        sequence

        integer(4) i
        real(8) r
    end type

    type(seq1), intent(inout) :: s(*)

    s(1:3) = (/(seq1 (i, 1.5_8*i), i=1, 3)/)
end subroutine
