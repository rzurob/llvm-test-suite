! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn093f.f
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
! %POSTCMD: dcomp ftybn093f.f ftybn093f.vf 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn093f.f 
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
!*                               condition: if the overriden binding is 
!*                               pure then the overriding binding shall
!*                               also be pure.
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

      type, extends(base) :: parent    ! (20,4)
      contains
!* expect error message 1514-570
         procedure, nopass :: bind_b => proc2
      end type  

      contains
      pure function proc1()
         proc1 = 100
      end function 

      function proc2()
         proc2 = 200
      end function

   end module     

   end
   
