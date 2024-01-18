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
! %GROUP: fext033.f
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
!*  DATE                       : Nov. 10, 2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : type extension (base type contains sequence
!*                               type component)
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
    type seq1(k1,k2)
        integer, kind :: k1,k2
        sequence
        integer(k1) :: i1
        integer(k2) :: i2
    end type

    type base(k3)
        integer, kind :: k3
        integer(k3) :: id
        type(seq1(k3,k3)) :: s1
    end type
end module
use m

    type, extends(base) :: child(n)
        integer, len :: n
        character(n) :: name
    end type
    logical isSeq1Valid

    type (child(4,20)) :: c1
    type (base(4)) :: b1

    c1%s1%i1 = 1
    c1%base%s1%i2 = 2

    c1%id = 100
    c1%name = 'test data c1'


    b1%id = 10
    b1%s1%i1 = -1
    b1%s1%i2 = -2

    if (.not. isSeq1Valid (c1%s1, 1_4, 2_4)) error stop 1_4
    if (.not. isSeq1Valid (c1%base%s1, 1_4, 2_4)) error stop 2_4

    if ((c1%base%id /= 100) .or. (c1%id /= c1%base%id)) error stop 3_4

    if (c1%name /= 'test data c1') error stop 4_4

    if (.not. isSeq1Valid (b1%s1, -1_4, -2_4)) error stop 5_4

    if (b1%id /= 10) error stop 6_4
end

logical function isSeq1Valid (s, intVal1, intVal2)
    type seq1(k1,k2)
        integer, kind :: k1,k2
        sequence
        integer(k1) :: i1
        integer(k2) :: i2
    end type

    type (seq1(4,4)), intent(in) :: s
    integer(4), intent(in) :: intVal1, intVal2

    isSeq1Valid = ((s%i1 .eq. intVal1) .and. (s%i2 .eq. intVal2))
end function
