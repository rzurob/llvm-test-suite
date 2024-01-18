! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/dummy_arg/fArg005d11.f
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
! %POSTCMD: dcomp fArg005d11.f
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
!*  DATE                       : 05/06/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : argument association (for pointer and
!*                               allocatable dummy-args the ranks of the
!*                               actual-args shall agree with dummy)
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
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
    end type

    contains

    subroutine scalarPointer (b)
        class (base(4,:)), pointer :: b
    end subroutine

    subroutine scalarAllocatable (b)
        class (base(4,:)), allocatable :: b
    end subroutine

    subroutine rankOnePointer (b)
        class (base(4,:)), pointer :: b(:)
    end subroutine

    subroutine rankOneAllocatable (b)
        class (base(4,:)), allocatable :: b(:)
    end subroutine
end module

program fArg005d11
use m
    class (base(4,:)), pointer :: b1, b2(:), b3(:,:)
    class (base(4,:)), allocatable :: ba1, ba2(:), ba3(:,:)

    !! each of the following calls will receive an error message

    call scalarPointer (b2)
    call scalarPointer (b3)

    call scalarAllocatable (ba2)
    call scalarAllocatable (ba3)

    call rankOnePointer (b1)
    call rankOnePointer (b3)

    call rankOneAllocatable (ba1)
    call rankOneAllocatable (ba3)
end
