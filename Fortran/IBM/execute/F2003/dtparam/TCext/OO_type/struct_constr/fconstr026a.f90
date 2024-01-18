! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr026a.f
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
    type base(k1,k2)    ! (4,4)
        integer, kind        :: k1,k2
        integer(k1), pointer :: id
        real(k2)             :: value
    end type

    type, extends(base) :: child(k3,n1)    ! (4,4,1,20)
        integer, kind             :: k3
        integer, len              :: n1
        character(kind=k3,len=n1) :: name
    end type

    type (base(4,4)) :: b1_m = base(4,4) (id = null(), value = 1.0)
    type (child(4,4,1,20)) :: c1_m = child(4,4,1,20) (id = null(), value=2.0, name = 'c1_m')
end module


program fconstr026a
use m

    integer*4, target :: x = 100
    type (base(4,4)) :: b1
    type (child(4,4,1,20)) :: c1

    b1 = base(4,4) (x, 3.0)
    c1 = child(4,4,1,20) (name = 'c1', base = base(4,4)(id = x, value = 4.0))

    ! validate variables
    if (associated (b1_m%id) .or. (b1_m%value /= 1.0)) error stop 1_4

    if (associated (c1_m%id) .or. (c1_m%value /= 2.0) .or. &
         (c1_m%name /= 'c1_m') ) error stop 2_4

    if ((b1%id /= 100) .or. (b1%value /= 3.0)) error stop 3_4

    if ((c1%id /= 100) .or. (c1%value /= 4.0) .or. &
        (c1%base%id /= 100) .or. c1%name /= 'c1') error stop 4_4


    b1 = base(4,4) (x, value = 3.0)
    if ((b1%id /= 100) .or. (b1%value /= 3.0)) error stop 5_4

    b1 = base(4,4) (id = x, value = 3)
    if ((b1%id /= 100) .or. (b1%value /= 3.0)) error stop 6_4

    c1 = child(4,4,1,20) (name = 'c3', base = base(4,4)(x, 5.0))

    if ((c1%id /= 100) .or. (c1%value /= 5.0) .or. &
        (c1%base%id /= 100) .or. c1%name /= 'c3') error stop 7_4

end
