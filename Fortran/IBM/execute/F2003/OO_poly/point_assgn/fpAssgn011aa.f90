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
! %GROUP: fpAssgn011aa.f
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
!*  DATE                       : 02/10/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*                                                                     
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DRIVER STANZA              : xlf90
!*
!*  DESCRIPTION                : pointer assignment (base self-assignment: array
!*                               assigned to a section derived from itself)
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

program fpAssgn011aa
    integer*4, target :: i1(101:300)

    integer*4, pointer :: i_ptr(:)

    i1 = (/(i, i=101,300)/)

    i_ptr => i1

    if ((lbound (i_ptr, 1) /= 101) .or. (ubound(i_ptr, 1) /= 300)) error stop 1_4

    i_ptr => i_ptr(::2)

    if (size(i_ptr) /= 100) error stop 2_4

    if ((lbound (i_ptr, 1) /= 1) .or. (ubound(i_ptr, 1) /= 100)) error stop 3_4

    if (.not. associated (i_ptr, i1(::2))) error stop 4_4

    if ((i_ptr (2) /= 103) .or. (i_ptr (50) /= 199))  error stop 5_4

end
