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
!*  DATE                       : 12/20/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.6.1: inheritance)
!                               Case: base type cotains parameterized sequence
!                               type.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type seq1 (k, l)
        integer, kind :: k
        integer, len  :: l

        sequence

        integer(k) i1
        real(2*k) data
        character(l) name
    end type

    type base (k1, ks, ls)
        integer, kind :: k1, ks
        integer, len  :: ls

        integer(k1) :: id
        type(seq1(ks, ls)) :: val
    end type
end module

module m1
use m
    type, extends(base) :: child
        character(ls) :: name = 'default'
    end type
end module

program dtparamExtends030
use m1
    interface
        logical function isSeq1Valid2 (s, i1, data, name)
            type seq1 (k, l)
                integer, kind :: k
                integer, len  :: l

                sequence

                integer(k) i1
                real(2*k) data
                character(l) name
            end type

            type (seq1(2,*)), intent(in) :: s
            integer(2), intent(in) :: i1
            real(4), intent(in) :: data
            character(*), intent(in) :: name
        end function isSeq1Valid2

        logical function isSeq1Valid4 (s, i1, data, name)
            type seq1 (k, l)
                integer, kind :: k
                integer, len  :: l

                sequence

                integer(k) i1
                real(2*k) data
                character(l) name
            end type

            type (seq1(4,*)), intent(in) :: s
            integer(4), intent(in) :: i1
            real(8), intent(in) :: data
            character(*), intent(in) :: name
        end function isSeq1Valid4
    end interface

    type (child(8, 2, 20)) c1
    type (child(4, 4, 16)) c2

    if ((c1%name /= 'default') .or. (c2%name /= 'default')) error stop 1_4

    c1%id = 2_8**33
    c1%val%i1 = -1
    c1%val%data = 2.12e0
    c1%val%name = 'xlftest 11.1 release'


    c2%id = 10
    c2%val%i1 = 2**20
    c2%val%data = 7.34d0
    c2%val%name = 'This is a test for release 11.1'

    c2%name = 'test for '//c1%val%name

    !! verify the data
    if (.not. isSeq1Valid2 (c1%val, -1_2, 2.12e0, 'xlftest 11.1 release')) &
                error stop 2_4

    if (.not. isSeq1Valid4 (c2%val, 2**20, 7.34d0, 'This is a test f')) &
                error stop 3_4

    if (c1%id /= 2_8**33) error stop 4_4
    if (c2%id /= 10) error stop 5_4

    if (c2%name /= 'test for xlftest') error stop 6_4
end


logical function isSeq1Valid2 (s, i1, data, name)
    type seq1 (k, l)
        integer, kind :: k
        integer, len  :: l

        sequence

        integer(k) i1
        real(2*k) data
        character(l) name
    end type

    type (seq1(2,*)), intent(in) :: s
    integer(2), intent(in) :: i1
    real(4), intent(in) :: data
    character(*), intent(in) :: name

    logical(4) precision_r4

    isSeq1Valid2 = (i1 == s%i1) .and. (name == s%name) .and. &
                    precision_r4 (data, s%data)
end function

logical function isSeq1Valid4 (s, i1, data, name)
    type seq1 (k, l)
        integer, kind :: k
        integer, len  :: l

        sequence

        integer(k) i1
        real(2*k) data
        character(l) name
    end type

    type (seq1(4,*)), intent(in) :: s
    integer(4), intent(in) :: i1
    real(8), intent(in) :: data
    character(*), intent(in) :: name

    logical(4) precision_r8

    isSeq1Valid4 = (i1 == s%i1) .and. (name == s%name) .and. &
                    precision_r8 (data, s%data)
end function
