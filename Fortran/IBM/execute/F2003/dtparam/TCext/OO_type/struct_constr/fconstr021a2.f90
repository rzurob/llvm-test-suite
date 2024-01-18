! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp /tstdev/OO_type/struct_constr/fconstr021a2.f
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
! %GROUP: fconstr021a2.f
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
!*  DATE                       :
!*  ORIGIN                     :
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (default
!*                               initialization for pointer comp.; omitted in
!*                               the structure constructor)
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
        integer, kind           :: k1
        integer(k1)             :: id = 1
        type(base(k1)), pointer :: next => null()
    end type

    type base1(k2,n1)    ! (4,20)
        integer, kind              :: k2
        integer, len               :: n1
        type(base1(k2,:)), pointer :: next => null()
    end type
end module

program fconstr021a2
use m

    type (base(4)) :: b1 = base(4) ()
    type (base(4)) :: b2 = base(4) (id = 10)

    type (base1(4,20)) :: bb1 = base1(4,20)()
    type (base(4)), pointer :: b_ptr

    if ((b1%id /= 1) .or. associated(b1%next)) error stop 1_4

    if ((b2%id /= 10) .or. associated(b2%next)) error stop 2_4

    if (associated(bb1%next)) error stop 3_4

    allocate(b_ptr)

    b_ptr = base(4) ()

    if (associated(b_ptr%next) .or. (b_ptr%id /= 1)) error stop 4_4

    deallocate(b_ptr)
end
