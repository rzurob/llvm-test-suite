! GB DTP extension using:
! ftcx_dtp -ql /tstdev/OO_tpbnd/specbnd/ftybn093aa.f
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
! %POSTCMD: dcomp ftybn093aa.f 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn093a.f 
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
!*                               condition: both shall be subroutines 
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
      end type base

      type, extends(base) :: parent    ! (20,4)
      end type  

      type, extends(parent) :: child    ! (20,4)
      contains
!* expect the error massage 1514-631
         procedure, nopass  :: bind_b => proc3
      end type
  
      contains
      subroutine proc1()
      end subroutine
     
      integer function proc3()
         proc3 = 100
      end function

   end module     
   
   end
   
