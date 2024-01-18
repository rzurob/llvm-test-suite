! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn093c.f
! opt variations: -qnol

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
! %POSTCMD: dcomp ftybn093c.f ftybn093c.vf 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn093c.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute 
!*
!*  SECONDARY FUNCTIONS TESTED : overriding 
!*
!*  DESCRIPTION                : The overriding binding and the overriden 
!*                               binding shall satisfy the following
!*                               condition: both shall be functions having
!*                               the same result characteristics.
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
      contains
         procedure, nopass :: bind_b => proc2
      end type  

      type, extends(base) :: parent2    ! (20,4)
      contains
         procedure, nopass  :: bind_b => proc3
      end type
  
      contains
      function proc1() result(arg1)
         integer :: arg1
         arg1 = 100
      end function 
     
      function proc2() result(arg1)
         character*20 :: arg1
         arg1 = "hello" 
      end function 

      subroutine proc3()
      end subroutine 

   end module     

   end
   
