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
! %GROUP: fconstr026a.f
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
!*  DATE                       : 12/22/2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (pointer components
!*                               in structure constructor)
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
        integer*4, pointer :: id
        real*4 :: value
    end type

    type, extends(base) :: child
        character(20) :: name
    end type

    type (base) :: b1_m = base (id = null(), value = 1.0)
    type (child) :: c1_m = child (id = null(), value=2.0, name = 'c1_m')
end module


program fconstr026a
use m

    integer*4, target :: x = 100
    type (base) :: b1
    type (child) :: c1

    b1 = base (x, 3.0)
    c1 = child (name = 'c1', base = base(id = x, value = 4.0))

    ! validate variables
    if (associated (b1_m%id) .or. (b1_m%value /= 1.0)) error stop 1_4

    if (associated (c1_m%id) .or. (c1_m%value /= 2.0) .or. &
         (c1_m%name /= 'c1_m') ) error stop 2_4

    if ((b1%id /= 100) .or. (b1%value /= 3.0)) error stop 3_4

    if ((c1%id /= 100) .or. (c1%value /= 4.0) .or. &
        (c1%base%id /= 100) .or. c1%name /= 'c1') error stop 4_4


    b1 = base (x, value = 3.0)
    if ((b1%id /= 100) .or. (b1%value /= 3.0)) error stop 5_4

    b1 = base (id = x, value = 3)
    if ((b1%id /= 100) .or. (b1%value /= 3.0)) error stop 6_4

    c1 = child (name = 'c3', base = base(x, 5.0))

    if ((c1%id /= 100) .or. (c1%value /= 5.0) .or. &
        (c1%base%id /= 100) .or. c1%name /= 'c3') error stop 7_4

end
