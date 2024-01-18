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
! %GROUP: fconstr026.f
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
!*  DESCRIPTION                : structure constructor (pointer component
!*                               initialization, pointer of intrinsic type)
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

    type, extends(child) :: thirdGeneration
        logical*1 :: isSet
    end type

    integer*4, target :: x_m = 10

    type (base) :: b1_m = base (null(), 1.0)
    type (child) :: c1_m = child (name = 'c1_m', base = base &
                    (id = null(), value = 2.0))

    type (thirdGeneration) :: t1_m = thirdGeneration (isSet = .true., &
         child = child (base = base (null(), value = 3.0), name = 't1_m'))


    contains

    subroutine updateModuleData
        b1_m = base (value = 1.0, id = x_m)

        c1_m = child (x_m, value = 1.0, name = 'c1_m')

        t1_m = thirdGeneration (name = 't1_m', base = base (x_m, 1.0), &
                isSet = (x_m == 10))
    end subroutine
end module


program fconstr026
use m

    integer*4, target :: x = 100
    type (base) :: b1
    type (child) :: c1, c2
    type (thirdGeneration) :: t1

    b1 = base (id = null(), value = 3)
    c1 = child (name = 'c1', base = base(id = x, value = 4.0))
    t1 = thirdGeneration (name = 't1', isSet = t1_m%isSet, base = b1_m)

    c2 = child (name = 'c2', base = base(x, 5.0))

    ! validate all objects

    if (associated(b1_m%id) .or. (b1_m%value /= 1.0)) error stop 1_4

    if (associated(c1_m%id) .or. (c1_m%value /= 2.0) .or. &
        (c1_m%name /= 'c1_m'))  error stop 2_4

    if (associated(t1_m%id) .or. (t1_m%value /= 3.0) .or. &
        (t1_m%name /= 't1_m') .or. (.not. t1_m%isSet)) error stop 3_4

    if (associated(b1%id) .or. (b1%value /= 3.0)) error stop 4_4

    if ((c1%id /= 100) .or. (c1%value /= 4.0) .or. (c1%name /= 'c1')) &
            error stop 5_4

    if ((.not. associated (c2%id, x)) .or. (c2%value /= 5.0) .or. &
            (c2%name /= 'c2')) error stop 6_4

    if (associated(t1%id) .or. (t1%value /= 1.0) .or. (t1%name /='t1') .or. &
        (.not. t1%isSet)) error stop 7_4


    call updateModuleData

    ! validate the module data again
    if ((b1_m%id /= 10) .or. (b1_m%value /= 1.0)) error stop 8_4

    if ((.not. associated(c1_m%id, x_m)) .or. (c1_m%value /= 1.0) .or. &
            (c1_m%name /= 'c1_m')) error stop 9_4

    if ((.not. associated(t1_m%id, x_m)) .or. (t1_m%value /= 1.0) .or. &
            (t1_m%name /= 't1_m') .or. (.not. t1_m%isSet)) error stop 10_4
end
