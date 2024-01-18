! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qreuse=none /tstdev/OO_tpbnd/specbnd/ftybn020l.f
! opt variations: -qnok -qnol -qdefaultpv -qreuse=self

!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS: -qfree=f90 
! %GROUP: ftybn020l.f
! %VERIFY: 
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn020l.f
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
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

   type, extends(parent) :: child(k2,n2)    ! (20,4,4,20)
       integer, kind :: k2
       integer, len  :: n2
   end type

   type dt(k3,n3,n4)    ! (4,20,3)
      integer, kind            :: k3
      integer, len             :: n3,n4
      type(child(n3,k3,k3,n3)) :: dt_c(n4)
   end type

   type(parent(20,4)) :: dt_p(2)
   type(dt(4,20,3)) :: dt_test(3)

   contains
      subroutine proc1(arg1)
         class(parent(*,4)) :: arg1
         arg1%x = 100 
      end subroutine
   subroutine test
      call dt_p(1)%bind() 
      call dt_test(1)%dt_c(3)%bind()
      if (dt_p(1)%x .ne. 100)  error stop 2_4
      if (dt_test(1)%dt_c(3)%x .ne. 100)  error stop 3_4
   end subroutine

   end module

   use mod1

   call test

   end
