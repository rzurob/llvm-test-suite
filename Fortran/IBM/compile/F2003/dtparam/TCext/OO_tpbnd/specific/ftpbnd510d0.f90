! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=self -qreuse=base /tstdev/OO_tpbnd/specific/ftpbnd510d0.f
! opt variations: -qck -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
! %GROUP: redherring.f
! %VERIFY: 
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp ftpbnd510d0.f
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
!*  DATE                       : 05/12/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : specific type bound (subobject of a data with PROTECTED
!*                               or PARAMETER attributes still has that
!*                               attribute)
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
    type base(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)   :: id

        contains

        procedure :: assgnID => assignIDVal
        procedure, nopass :: assgnBase
    end type

    type, extends (base) :: child    ! (20,4)
        character(n1) :: name
    end type

    type container(k2,n2)    ! (4,20)
        integer, kind              :: k2
        integer, len               :: n2
        class(base(:,k2)), pointer :: data => null()
    end type

    type (child(20,4)), protected :: c1 = child(20,4) (1, 'c1_protected')
    type (container(4,20)), protected, save :: co1

    type (child(20,4)), parameter :: c2 = child(20,4) (2, 'c2_const')

    contains

    subroutine assgnBase (b, id)
        class (base(:,4)), pointer, intent(out) :: b
        integer*4, intent(in) :: id

        allocate (b, source=base(20,4)(id))
    end subroutine

    subroutine assignIDVal (b, id)
        class (base(*,4)), intent(inout) :: b
        integer*4, intent(in) :: id

        b%id = id
    end subroutine
end module

program ftpbnd510d0
use m
    class (base(:,4)), pointer :: b

    call c1%base%assgnID (10)   !<-- this is illegal

    call c2%base%assgnID (20)   !<-- this is illegal

    co1%data => b       !<-- this is illegal

    call co1%data%assgnBase (co1%data, 15)  !<-- this is illegal
end
