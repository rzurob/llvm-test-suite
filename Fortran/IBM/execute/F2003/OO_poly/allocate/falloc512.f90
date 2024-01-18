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
!*  DATE                       : 02/03/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : allocate (allocate of sequence types with
!                               source-expr that refers to a sequence type that
!                               is pointer assigned to unlimted poly target)
!*
!*  KEYWORD(S)                 :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program falloc512
    type seq1
        sequence
        integer(8) i
        real(4) r
    end type

    type (seq1), pointer :: s1(:), s2(:)

    class (*), allocatable, target :: x1(:)

    allocate (x1(3), source=(/(seq1(i, i*1.1), i = 1, 3)/))

    s1 => x1

    allocate (s2(size(s1)), source=s1)

    write (*, '(i8, f10.2)') s2

    deallocate (s2)

    print *, ''

    allocate (s2(2), source=s1(::2))
    write (*, '(i8, f10.2)') s2

    print *, ''

    deallocate (s2)

    s1 => x1(3:1:-2)

    allocate (s2(size(s1)), source=s1)
    write (*, '(i8, f10.2)') s2
end
