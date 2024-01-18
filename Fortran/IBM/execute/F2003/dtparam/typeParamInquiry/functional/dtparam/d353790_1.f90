!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : d353790_1.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 28 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3 
!* 2. DEFECT 353790 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
   module m
     type base(k)
          integer,kind :: k
     end type

   contains
      function fun1(b)
         type(base(2)) :: b 
         integer(b%k) fun1
         fun1=kind(fun1) + b%k * 10
      end function

   end module
    program d353790_1
    use m
    implicit none

    type(base(2)) :: b
    print *,fun1(b)

    end

