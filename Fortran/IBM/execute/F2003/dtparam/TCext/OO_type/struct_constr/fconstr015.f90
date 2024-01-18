! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr015.f
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
! %GROUP: fconstr015.f
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
!*  DATE                       : Nov. 14, 2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (generics takes higher
!*                               precedence)
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
        integer(k1)   :: id = 10
    end type

    type, extends(base) :: child(k2,n1)    ! (4,1,20)
        integer, kind             :: k2
        integer, len              :: n1
        character(kind=k2,len=n1) :: name = 'default'
    end type

end module

program fconstr015
use m

    interface child
        type (child(4,1,20)) function c_c()
        use m
        end function
    end interface

    type (child(4,1,20)) :: c1, c2, c3

    c1 = child()  ! this is call interface
    c3 = child(4,1,20)(100, 'data c3')  ! this is structure constructor

    if (c1%id /= 1) error stop 1_4
    if (c1%name /= 'test') error stop 2_4

    if (c2%id /= 10) error stop 3_4
    if (c2%name /= 'default') error stop 4_4

    if (c3%id /= 100) error stop 5_4
    if (c3%name /= 'data c3') error stop 6_4
end

type (child(4,1,20)) function c_c()
use m

    c_c = child(4,1,20)(1, 'test')
end function

