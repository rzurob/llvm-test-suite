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
! %GROUP: fconstr039.f
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
!*  DATE                       : 02/24/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (struct_constr in forall
!*                               construct)
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
    type base
        integer*4 :: id
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type (base), allocatable :: b1_m(:)
end module

program fconstr039
use m
    type (child), pointer :: c1(:)
    character*20 :: names (10)

    names = (/'c1_m_0', 'c1_m_1', 'c1_m_2', 'c1_m_3', 'c1_m_4', 'c1_m_5', &
             'c1_m_6', 'c1_m_7', 'c1_m_8', 'c1_m_9'/)

    allocate (b1_m(20), c1(10))

    forall (i=1:20)
        b1_m(i) = base(i+100)
    end forall

    forall (i=1:10)
        c1(i) = child (name='c1_m_'//char(ichar('0')+i-1), id = i)
    end forall

    do i = 1, 20
        if (b1_m(i)%id /= 100+i) error stop 1_4
    end do

    do i = 1, 10
        if ((c1(i)%id /= i) .or. (c1(i)%name /= names(i))) error stop 2_4
    end do

    deallocate (c1)
end
