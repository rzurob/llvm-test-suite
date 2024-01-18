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
! %GROUP: fext018.f
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
!*  DATE                       : Nov. 07, 2003
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : derived-type extension (base type is renamed
!*                               via use association. Test base type name)
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
end module

module m1
use m, newBase => base

    type, extends(newBase) :: child
        character(20) :: name
    end type

    type (child) :: c1_m
    type (newBase) :: b1_m

end module

program fext018
    use m1

    type (newBase) :: b1
    type (child) :: c1

    b1%id = 10
    b1_m%id = 15

    c1%newbase%id = 20
    c1%name = 'Test1'

    c1_m%id = 25
    c1_m%name = 'c1_m'


    if (b1%id /= 10) error stop 1_4
    if (c1%id /= 20) error stop 2_4
    if (c1%name /= 'Test1') error stop 3_4

    c1%id = 100
    if (c1%newbase%id /= 100) error stop 4_4

    if (b1_m%id /= 15) error stop 5_4
    if (c1_m%name /= 'c1_m') error stop 6_4
    if (c1_m%id /= 25) error stop 7_4
    if (c1_m%id /= c1_m%newbase%id) error stop 8_4
end
