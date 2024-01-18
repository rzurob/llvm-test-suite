! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_tpbnd/specbnd/ftybn092g.f
! opt variations: -ql

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
! %POSTCMD: dcomp ftybn092g.f ftybn092g.vf 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ftybn092g.f 
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : nopass binding attribute 
!*
!*  SECONDARY FUNCTIONS TESTED : accessibility 
!*
!*  DESCRIPTION                : testing the accessiblity of parent's 
!*                               type-bound procedures are overrided by
!*                               the multiple generations extended types.
!*                             
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      module mod	      
  
      type base(k1)    ! (4) 
         integer, kind :: k1
         integer(k1)   :: x
	 contains
      	 procedure, nopass :: bind_b => proc1
      end type 

      type, extends(base) :: parent    ! (4)
		contains
!* no error message here
      	procedure, nopass, public :: bind_b => proc1
      end type  

      type, extends(parent) :: child    ! (4) 
      contains
!* expected an error message here
         procedure, nopass, private :: bind_b => proc1
      end type

      contains
      subroutine proc1()
      end subroutine

   end module     


   end
   
