! GB DTP extension using:
! ftcx_dtp -qck -qnol -qnodefaultpv /tstdev/OO_poly/point_assgn/fpAssgn006a.f
! opt variations: -qnock -ql -qdefaultpv

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
! %GROUP: fpAssgn006a.f
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
!*  DATE                       : 01/16/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : pointer assignment (pointer assignment for
!*                               pointer component takes place during intrinsic
!*                               assignment of a derived type)
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id = 1
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'no-name'
    end type
end module

program fpAssgn006a
use m
    type container(k3)    ! (4)
        integer, kind            :: k3
        class(base(k3)), pointer :: data => null()
    end type

    type (container(4)) :: co, co1

    type (base(4)), target :: b1 = base(4) (10)
    type (child(4,1,20)), target :: c1

    type (container(4)), pointer :: c_ptr1
    type (container(4)), allocatable :: c_alloc

    co = container(4) (data = c1)

    co1 = co

    if (.not. associated (co1%data, c1)) error stop 1_4
    if (.not. associated (co%data, c1)) error stop 2_4

    co = container(4) (data = b1)

    co1 = co

    if ((.not. associated (co1%data, b1)) .or. (co1%data%id /= 10)) error stop 3_4
    if (.not. associated (co%data, b1)) error stop 4_4

    nullify (co%data)

    co1 = co

    if (associated(co%data) .or. associated(co1%data)) error stop 5_4

    allocate (c_ptr1, c_alloc)
    if (associated (c_ptr1%data) .or. associated (c_alloc%data)) error stop 6_4

    c_ptr1 =  co1

    if (associated (c_ptr1%data)) error stop 7_4

    co%data => c1

    c_alloc = co

    if (.not. associated (c_alloc%data, co%data)) error stop 8_4
    if (c_alloc%data%id /= 1) error stop 9_4
end
