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
! %GROUP: fconstr047a.f
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
!*  DATE                       : 04/13/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : structure constructor (with default
!*                               initialization)
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
    type A
        integer*4 :: id = 0
        character*20 :: name = ''
        logical :: flag = .false.
    end type

    type B
        integer*4, pointer :: data => null()

        type(A) :: attr = A (name = 'default')
    end type
end module

program fconstr047a
use m
    type (B) :: b1
    type (B) :: b2 = B (attr = A ())

    integer*4, target :: i1

    i1 = 1000

    if ((b1%attr%id /=0) .or. (b1%attr%name /= 'default') .or. b1%attr%flag &
        .or. (associated (b1%data))) error stop 1_4


    if ((b2%attr%id /=0) .or. (b2%attr%name /= '') .or. b2%attr%flag &
        .or. (associated (b2%data))) error stop 2_4


    b1 = B (i1)

    if ((b1%attr%id /=0) .or. (b1%attr%name /= 'default') .or. b1%attr%flag & 
        .or. (b1%data /= 1000)) error stop 3_4

    b2 = B (attr = A (name = 'no-name'))


    if ((b2%attr%id /=0) .or. (b2%attr%name /= 'no-name') .or. b2%attr%flag &
        .or. (associated (b2%data))) error stop 4_4


    b1 = B (i1, A (i1))

    if ((b1%attr%id /= 1000) .or. (b1%attr%name /= '') .or. b1%attr%flag &
        .or. (b1%data /= 1000)) error stop 5_4
end
