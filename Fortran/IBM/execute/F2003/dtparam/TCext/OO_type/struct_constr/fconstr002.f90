! GB DTP extension using:
! ftcx_dtp -qck -qk -ql /tstdev/OO_type/struct_constr/fconstr002.f
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
! %GROUP: fconstr002.f
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
!*  DATE                       : Nov. 12, 2003
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : structure constructor (default initialization
!*                               for private components)
!*                               pointer types are disassociated; allocatable
!*                               not allocated
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
        integer, kind              :: k1,k2
        integer(k1), private       :: id = 1
        real(k2), pointer, private :: value => null()
    end type

    type, extends(base) :: child(k3,n1,k4)    ! (4,4,1,20,8)
        integer, kind                      :: k3,k4
        integer, len                       :: n1
        character(kind=k3,len=n1), private :: name = 'child type'
        real(k4), allocatable, private     :: extendedValue (:)
    end type

    type (base(4,4)), save :: b1_m
    type (child(4,4,1,20,8)), save :: c1_m

    contains

    logical function isBaseCorrect (b, intVal)
        type(base(4,4)), intent(in) :: b
        integer*4, intent(in) :: intVal

        isBaseCorrect = ( (.not. associated (b%value)) .and. &
                          (b%id .eq. intVal) )
    end function

    logical function isChildCorrect (c, intVal, charVal)
        type (child(4,4,1,*,8)), intent(in) :: c
        integer*4, intent(in) :: intVal
        character(*), intent(in) :: charVal

        isChildCorrect = ( isBaseCorrect (c%base, intVal) .and. &
                           (.not. allocated (c%extendedValue)) .and. &
                           (c%name .eq. charVal) )
    end function
end module

program fconstr002
use m

    type, extends (base) :: secondChild(k5)    ! (4,4,4)
        integer, kind     :: k5
        real(k5), pointer :: extendedValue (:) => null()
    end type

    type (base(4,4)) :: b1
    type (child(4,4,1,20,8)) :: c1
    type (secondChild(4,4,4)) :: s1

    if (.not. isBaseCorrect (b1_m, 1_4)) error stop 1_4
    if (.not. isBaseCorrect (b1, 1_4)) error stop 2_4

    if (.not. isChildCorrect (c1_m, 1_4, 'child type')) error stop 3_4
    if (.not. isChildCorrect (c1, 1_4, 'child type')) error stop 4_4

    if (.not. isBaseCorrect (s1%base, 1_4)) error stop 5_4
    if (associated (s1%extendedValue)) error stop 6_4
end
