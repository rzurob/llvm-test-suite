! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/volatile/test/volatileC527Value.f
! opt variations: -ql

!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 30/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : VALUE attribute, VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  C527
!*
!*   C526: if the value attribute is specified, the ....VOLATILE attribute
!*         shall not be specified.
!* ===================================================================

  module mod
    implicit none
    type tt(k1)    ! (4)
      integer, kind :: k1
      integer(k1)      x(2,2)
    end type
  end module

  program volatileC527Value 
     use mod
     implicit none
     interface
       subroutine sub(xx)
         use mod
         implicit none
         type(tt(4)), value :: xx
         VOLATILE xx
       end subroutine
     end interface

  end program volatileC527Value 

