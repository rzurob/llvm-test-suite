! GB DTP extension using:
! ftcx_dtp -ql -qreuse=base /tstdev/OO_tpbnd/specbnd/ftybn092f.f
! opt variations: -qnol -qreuse=none

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
! %POSTCMD: dcomp ftybn092f.f ftybn092f.vf
! %END
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn092f.f
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute
!*
!*  SECONDARY FUNCTIONS TESTED : overriding
!*
!*  DESCRIPTION                : testing accessiblity overriding with two
!*                               types which all extend from the base
!*                               type, but overriding the type-bound
!*                               procedures of the base type differently.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod

      type base(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         integer(k1)   :: x
	 contains
      	 procedure, nopass :: bind_b => proc1
      end type

      type, extends(base) :: parent1    ! (20,4)
         integer(k1) :: y
      contains
         procedure, nopass :: bind_b => proc1
      end type

      type, extends(base) :: parent2    ! (20,4)
         integer(k1) :: z
      contains
!* expected the error message 1514-588 (S) here
         procedure, nopass,private  :: bind_b => proc1
      end type

      type(base(20,4)) :: dt

      contains
      subroutine proc1()
      end subroutine
      end module

   end

