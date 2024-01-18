! GB DTP extension using:
! ftcx_dtp -qck -ql -qnodeferredlp -qreuse=base /tstdev/OO_poly/point_assgn/fpAssgn024.f
! opt variations: -qnock -qnol -qdeferredlp -qreuse=none

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
! %GROUP: fpAssgn024.f
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
!*  DATE                       : 03/26/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : data pointer assignment (non-poly pointer array
!*                               assigned to poly-pointer array; TARGET is the
!*                               parent component; use rank-one arrays)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id = 0
    end type

    type, extends (base) :: child(k2)    ! (20,4,1)
        integer, kind             :: k2
        character(kind=k2,len=n1) :: name = ''
    end type

    type (child(20,4,1)), target, allocatable :: c1_m(:)
end module

program fpAssgn024
use m
    type (child(20,4,1)), target :: c1(10)
    type (child(20,4,1)), target :: c2(3,3)

    class (base(20,4)), pointer :: b_ptr(:)
    type (base(20,4)), pointer :: b1(:)

    allocate (c1_m(20))

    c1_m%id = (/(i+10, i=1, 20)/)
    c1_m%name = 'c1_m'

    c1 = (/(child(20,4,1)(i,name='c1'), i=1,10)/)

    c2 = reshape ((/(child(20,4,1)(i, name='c2'),i=31,39)/), (/3,3/))

    !! transfer c1_m%base to b1 through poly-pointer
    b_ptr => c1_m
    b1 => b_ptr

    if ((size (b1) /= 20) .or. (.not. associated (b1, c1_m%base))) error stop 1_4

    if (any (b1%id /= (/(i,i=11,30)/))) error stop 2_4

    !! transfer c1(::2)%base to b1 through poly-pointer
    b_ptr => c1

    b1 => b_ptr(::2)

    if ((size(b1) /= 5) .or. (.not. associated(b1, c1(::2)%base))) error stop 3_4

    if (any (b1%id /= (/(i,i=1,10,2)/))) error stop 4_4

    !! transfer c2(2,::2)%base to b1 through poly-pointer
    b_ptr => c2 (2, :)

    b1 => b_ptr (::2)

    if ((size(b1) /= 2) .or. (.not. associated (b1, c2(2,::2)%base))) error stop 5_4

    if ((b1(1)%id /= 32) .or. (b1(2)%id /= 38)) error stop 6_4
end
