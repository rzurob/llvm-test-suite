! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/dtybn010.f
! opt variations: -ql

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : procedure name
!*
!*  SECONDARY FUNCTIONS TESTED : nopass, non_overridable
!*
!*  DESCRIPTION                : if neither =>procedure-name nor interface-name
!*                               appears, it is as though =>procedure-name
!*                               had appeared with a procedure name the same
!*                               as the binding name.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, private, nopass, non_overridable :: bind
      end type

      type(parent(4)) :: dt_p

      contains
      subroutine bind(arg1)
         class(parent(4)) :: arg1
      end subroutine

   subroutine test
      call dt_p%bind(dt_p)
   end subroutine

   end module

   use mod1

   call test

   end
