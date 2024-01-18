! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/class/fclass011.f
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
! %GROUP: fclass011.f
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
!*  DATE                       : 12/21/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : CLASS keyword (unlimited poly allocatable
!                               component in intrinsic assignment; using integer
!                               type as the dynamic types)
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

program fclass011
    type base(k1,n1)    ! (4,20)
        integer, kind :: k1
        integer, len  :: n1
        class (*), allocatable :: data(:)
    end type

    type (base(4,20)) b1, b2
    integer(8) i1(0:2)

    i1 = (/0, 1, 2/)

    allocate (b1%data(lbound(i1,1):ubound(i1,1)), source=i1)

    !! intrinsic assignment
    b2 = b1

    if (.not. allocated (b2%data)) error stop 1_4

    if ((lbound(b2%data,1) /= 0) .or. (ubound(b2%data, 1) /= 2)) error stop 2_4


    !! verify result
    select type (x => b2%data)
        type is (integer(8))
            if (any(x /= (/0_8, 1_8, 2_8/))) error stop 3_4
        class default
            error stop 4_4
    end select

    !! second test

    deallocate (b2%data)

    b1 = b2

    if (allocated (b1%data)) error stop 5_4

end
