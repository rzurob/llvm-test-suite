! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv /tstdev/OO_tpbnd/specbnd/ftybn020j.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=self

!**********************************************************************
!*  ===================================================================
!*
!*  DATE                       :
!*
!*  PRIMARY FUNCTIONS TESTED   : private type bound procedure
!*
!*  SECONDARY FUNCTIONS TESTED : pass, non_overridable
!*
!*  DESCRIPTION                : the accessiblity of a type-bound procedure
!*                               is not affected by a PRIVATE statement
!*                               in the component-part, the accessiblity
!*                               of a data component is not affected by a
!*                               PRIVATE statemnt in the type-bound-procedure
!*                               -part.
!*
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

   module mod1
      type parent(k1)    ! (4)
         integer, kind :: k1
         integer(k1)   :: x
	 contains
!        private
      	 procedure, pass, non_overridable :: bind => proc1
      end type

      type(parent(4)) :: dt_p

      contains
      subroutine proc1(arg1)
         class(parent(4)) :: arg1
         arg1%x = 100
      end subroutine

   subroutine test1
      call dt_p%bind()
   end subroutine

   end module

   use mod1
   type, extends(parent) :: child    ! (4)
   end type

   type dt(k2)    ! (4)
      integer, kind   :: k2
      type(child(k2)) :: dt_c
   end type

   type(dt(4)) :: dt_test

   call test1
   call dt_test%dt_c%bind()

   if (dt_p%x .ne. 100)  error stop 2_4
   if (dt_test%dt_c%x .ne. 100)  error stop 3_4

   end
