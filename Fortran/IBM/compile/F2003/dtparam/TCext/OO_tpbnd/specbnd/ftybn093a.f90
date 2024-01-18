! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn093a.f
! opt variations: -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp ftybn093a.f ftybn093a.vf
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn093a.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : The overriding binding and the overriden
!*                               binding shall satisfy the following
!*                               condition: both shall be subroutines
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod

      type base(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
      contains
      	 procedure, nopass :: bind_b => proc1
      end type base

      type, extends(base) :: parent1    ! (4)
      contains
         procedure, nopass :: bind_b => proc2
      end type

      type, extends(base) :: parent2    ! (4)
      contains
!* expect the error massage 1514-631
         procedure, nopass  :: bind_b => proc3
      end type

      contains
      subroutine proc1()
      end subroutine

      subroutine proc2()
      end subroutine

      integer function proc3()
         proc3 = 100
      end function

   end module

   end

