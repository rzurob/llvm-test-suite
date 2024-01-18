! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc512.f
! SCCS ID Information
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 02/03/2005
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
    type seq1(k1,k2)    ! (8,4)
        integer, kind :: k1,k2
        sequence
        integer(k1)      i
        real(k2)         r
    end type

    type (seq1(8,4)), pointer :: s1(:), s2(:)

    class (*), allocatable, target :: x1(:)

    allocate (x1(3), source=(/(seq1(8,4)(i, i*1.1), i = 1, 3)/))

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
