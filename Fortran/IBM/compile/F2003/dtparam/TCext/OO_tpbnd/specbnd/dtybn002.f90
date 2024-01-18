! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/dtybn002.f
! opt variations: -ql

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS:
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp dtybn002.f
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtybn002.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : nopass
!*
!*  DESCRIPTION                : a binding-private-stmt is permitted only
!*                               if the type definition is within the
!*                               specification part of a module.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure,private, nopass :: bind => proc1
      end type

      contains
      subroutine proc1(arg1)
         class(parent(4)) :: arg1
      end subroutine
   end module

   use mod1

   type(parent(4)) :: dt_p
   call dt_p%bind(dt_p)

   end

