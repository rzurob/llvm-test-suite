! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv /tstdev/OO_tpbnd/specbnd/ftybn020k.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

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
      type parent(n1,k1)    ! (20,4)
         integer, kind :: k1
         integer, len  :: n1
         private
         integer(k1)   :: x
	 contains
      	 procedure, pass, non_overridable :: bind => proc1
      end type

   type, extends(parent) :: child    ! (20,4)
   end type

   type dt(k2,n2)    ! (4,20)
      integer, kind      :: k2
      integer, len       :: n2
      type(child(n2,k2)) :: dt_c
   end type

   type(parent(20,4)) :: dt_p
   type(dt(4,20)) :: dt_test

   contains
      subroutine proc1(arg1)
         class(parent(*,4)) :: arg1
         arg1%x = 100
      end subroutine

   subroutine test
      call dt_p%bind()
      call dt_test%dt_c%bind()
      if (dt_p%x .ne. 100)  error stop 2_4
      if (dt_test%dt_c%x .ne. 100)  error stop 3_4
   end subroutine

   end module

   use mod1

   call test

   end
