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
!*  DATE                       : 01/24/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*
!*  DESCRIPTION                : dtparam (section 4.5.3: components)
!                               Case: Test the PASS attribute; subroutine and
!                               function; manipulation of strings.
!*
!*
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program dtparamProcComp007
use m
    type(string(:)), allocatable :: s1, s2

    allocate (s1, source=string(10)('xlftest   ', genString, findNreplace))

    call s1%replace(' ', '_')

    allocate (s2, source=s1%concat('team'))

    !! verify s1 and s2

    if ((s1%val /= 'xlftest___') .or. (.not. associated(s1%concat, genString)) &
            .or. (.not. associated(s1%replace, findNreplace))) error stop 1_4

    if ((s2%val /= 'xlftest___team') .or. associated(s2%concat) .or. &
        associated(s2%replace)) error stop 2_4
end
