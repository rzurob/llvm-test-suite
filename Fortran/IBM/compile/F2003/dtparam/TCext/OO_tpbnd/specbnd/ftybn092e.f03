! GB DTP extension using:
! ftcx_dtp -ql -qreuse=none /tstdev/OO_tpbnd/specbnd/ftybn092e.f
! opt variations: -qnol -qreuse=base

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : accessiblity
!*
!*  DESCRIPTION                : chang the accessibility of the
!*                               type-bound procedures by overriding
!*                               it in an extended type.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod

      type base(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
         contains
      	 procedure, nopass,public :: bind_b => proc1
      end type

      type, extends(base) :: child(n2,k2)    ! (20,4,20,4)
         integer, kind :: k2
         integer, len  :: n2
         integer(k2)   :: y
      contains
!* will issure an error message here
         procedure, nopass, private :: bind_b => proc1
      end type

      contains
      subroutine proc1()
      end subroutine

   end module


   end
