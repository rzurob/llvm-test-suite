! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_poly/allocate/falloc006a.f
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
! %GROUP: falloc006a.f
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
!*  DATE                       : 12/15/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : allocate (unlimited poly allocatable scalars as
!                               allocate-objects in allocate statement; use the
!                               sequence type or bind(C) type data as the
!                               source-expr)
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

program falloc006a
use iso_c_binding
    type seq1(k1)    ! (4)
        integer, kind :: k1
        sequence
        integer(k1)      i,j
    end type

    type, bind(C) :: bc1
        integer(c_int) :: i
        integer(c_short) :: j
    end type

    class (*), allocatable, target :: x0, x1(:)
    type (seq1(4)), pointer :: s0, s1(:)
    type (bc1), pointer :: b0, b1(:)

    !! test sequence type first
    allocate (x0, source=seq1(4)(10, 20))
    allocate (x1(-1:0), source=(/seq1(4)(1, 2), seq1(4)(10, 20)/))

    s0 => x0
    s1 => x1

    if ((s0%i /= 10) .or. (s0%j /= 20)) error stop 1_4

    if ((lbound(s1,1) /= -1) .or. (ubound(s1,1) /= 0)) error stop 2_4
    if (any(s1%i /= (/1,10/)) .or. any (s1%j /= (/2, 20/))) error stop 3_4

    !! test the BIND(C) type
    deallocate (x0, x1)

    allocate (x0, source=bc1(10, 20))
    allocate (x1(0:1), source=(/bc1(-1, -5), bc1(100, 500)/))

    b0 => x0
    b1 => x1

    if ((b0%i /= 10) .or. (b0%j /= 20)) error stop 4_4

    if ((lbound(b1,1) /= 0) .or. (ubound(b1, 1) /= 1)) error stop 5_4

    if (any (b1%i /= (/-1, 100/)) .or. any (b1%j /= (/-5, 500/))) error stop 6_4
end
