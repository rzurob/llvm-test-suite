! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr039.f
! SCCS ID Information
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
!*
!*  DATE                       : 02/24/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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
    type base(k1)    ! (4)
        integer, kind :: k1
        integer(k1)   :: id
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name
    end type

    type (base(4)), allocatable :: b1_m(:)
end module

program fconstr039
use m
    type (child(4,1,20)), pointer :: c1(:)
    character*20 :: names (10)

    names = (/'c1_m_0', 'c1_m_1', 'c1_m_2', 'c1_m_3', 'c1_m_4', 'c1_m_5', &
             'c1_m_6', 'c1_m_7', 'c1_m_8', 'c1_m_9'/)

    allocate (b1_m(20), c1(10))

    forall (i=1:20)
        b1_m(i) = base(4)(i+100)
    end forall

    forall (i=1:10)
        c1(i) = child(4,1,20) (name='c1_m_'//char(ichar('0')+i-1), id = i)
    end forall

    do i = 1, 20
        if (b1_m(i)%id /= 100+i) error stop 1_4
    end do

    do i = 1, 10
        if ((c1(i)%id /= i) .or. (c1(i)%name /= names(i))) error stop 2_4
    end do

    deallocate (c1)
end
