!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : ArrayConstructorEmpty09.f
!*
!*  PROGRAMMER                 : Rob James
!*  DATE                       : 09/14/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*
!*  DESCRIPTION                : Testing empty array constructors.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      @process check
      call sub(0)
      contains
        subroutine sub(n)
          integer i(n)
          i = [ integer(8) :: ]
        end subroutine
      end
