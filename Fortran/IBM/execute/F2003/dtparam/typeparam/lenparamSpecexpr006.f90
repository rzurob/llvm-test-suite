!#######################################################################
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 05/02/2007 (updated)
!*
!*  DESCRIPTION                : derived type parameter
!                               type parameter (Length type parameter may be
!                               used as specification expression in derived type
!                               definition: test sequence type.)
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program lenparamSpecexpr006
    type seq1 (l)
        integer, len :: l = 10

        sequence

        integer :: i, j(l)
    end type

    type seq2 (l, m)
        integer, len :: l, m

        sequence

        type (seq1(l)) :: s1
        type (seq1(l+m)) :: s2(l-m)
    end type

    type (seq2(10, 2)) :: s1

    type (seq2(20, 18)) :: s2

    interface
        subroutine setup (s, l, m)
            type seq1 (l)
                integer, len :: l = 10
                sequence
                integer :: i, j(l)
            end type

            type seq2 (l, m)
                integer, len :: l, m
                sequence
                type (seq1(l)) :: s1
                type (seq1(l+m)) :: s2(l-m)
            end type

            integer, intent(in) :: l, m
            type (seq2(l, m)), intent(out) :: s
        end subroutine

        subroutine printSeq2 (s, l, m)
            type seq1 (l)
                integer, len :: l = 10

                sequence

                integer :: i, j(l)
            end type

            type seq2 (l, m)
                integer, len :: l, m

                sequence

                type (seq1(l)) :: s1
                type (seq1(l+m)) :: s2(l-m)
            end type

            integer, intent(in) :: l, m
            type(seq2(l,m)), intent(in) :: s
        end subroutine
    end interface

    call setup (s1, 10, 2)
    call setup (s2, 20, 18)

    call printSeq2 (s1, 10, 2)
    call printSeq2 (s2, 20, 18)
end


subroutine setup (s, l, m)
    type seq1 (l)
        integer, len :: l = 10

        sequence

        integer :: i, j(l)
    end type

    type seq2 (l, m)
        integer, len :: l, m

        sequence

        type (seq1(l)) :: s1
        type (seq1(l+m)) :: s2(l-m)
    end type

    integer, intent(in) :: l, m
    type (seq2(l, m)), intent(out) :: s

    s%s1%i = -1
    s%s1%j = (/(i, i=1,l)/)

    s%s2%i = (/(-1-i, i=1,l-m)/)

    do i = 1, l-m
        s%s2(i)%j = (/(i*100+j, j = 1, l+m)/)
    end do
end subroutine


subroutine printSeq2 (s, l, m)
    type seq1 (l)
        integer, len :: l = 10

        sequence

        integer :: i, j(l)
    end type

    type seq2 (l, m)
        integer, len :: l, m

        sequence

        type (seq1(l)) :: s1
        type (seq1(l+m)) :: s2(l-m)
    end type

    integer, intent(in) :: l, m
    type(seq2(l,m)), intent(in) :: s

    print *, s
end subroutine

