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
!*  TEST CASE TITLE            : ArrayConstructorEmpty10.f
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
      type t
        integer i
      end type
      call sub(0)
      contains
        subroutine sub(n)
          type(t) i(n)
          i = [ t :: ]
        end subroutine
      end
