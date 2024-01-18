! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : underflowMode2.f
!*
!*  PROGRAMMER                 : Nancy Wang
!*  DATE                       : Nov. 15 2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_set_underflow_mode(gradual) 
!*                             : 
!*  SECONDARY FUNCTIONS TESTED :
!*
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                :    
!*  test if ieee_set_underflow_mode(gradual) conforms with F2003 standard based on argument perspective, pass an intrinsic function in SET subroutine  
!* 
!23456789012345678901234567890123456789012345678901234567890123456789012   
      
      program underflowMode2
         use,intrinsic :: ieee_arithmetic
         implicit none 

         logical :: underflowmode
         real :: r
         logical(2) :: l1_2=.true.,l2_2=.false.
         
!        pass an intrinsic function LOGICAL(L,KIND)to SET subroutine
         if(ieee_support_underflow_control(r)) then
            call ieee_set_underflow_mode(LOGICAL(l1_2,kind(underflowmode)))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .true.) error stop 101_4
            call ieee_set_underflow_mode(LOGICAL(l2_2,kind(underflowmode)))
            call ieee_get_underflow_mode(underflowmode)
            if(underflowmode .neqv. .false.) error stop 102_4
            error stop 103_4
         endif

      end program
