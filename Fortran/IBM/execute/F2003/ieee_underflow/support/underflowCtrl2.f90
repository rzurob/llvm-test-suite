! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : underflowCtrl2.f
!*
!*  PROGRAMMER                 : Nancy Wang
!*  DATE                       : Nov. 15 2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_support_underflow_control(X)
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*  REFERENCE                  : Feature Number 289080
!*
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                :    
!*  1. test if ieee_support_underflow_control(X) returns false when passing a
!*     default real type argument  
!*  2. test if ieee_support_underflow_control(X) returns false when passing a
!*     pointer of default real type argument 
!*  3. test if ieee_support_underflow_control(X) returns false when passing an  
!*     allocatable of default real type argument 
!*
!23456789012345678901234567890123456789012345678901234567890123456789012   
      module m
         use,intrinsic :: ieee_arithmetic
         real :: r
         double precision :: d
         real,pointer :: rp=>null()
         real,target  :: rt
         double precision,pointer :: dp=>null() 
         double precision,target :: dt
         real,allocatable :: ra
         double precision,allocatable :: da

      end module m
      program underflowCtrl2
         use m 
         implicit none
 
         if (.not. associated(rp)) rp=>rt
         if (.not. associated(dp)) dp=>dt 
         if (.not. allocated(ra))  allocate(ra)
         if (.not. allocated(da))  allocate(da)    
 
!        pass a default real type argument  
         if (ieee_support_underflow_control(r))  error stop 101_4

!        pass a double precision argument 
         if (ieee_support_underflow_control(d))  error stop 102_4

!        pass a pointer of real type argument
         if (ieee_support_underflow_control(rp)) error stop 103_4

!        pass a pointer of double precision argument
         if (ieee_support_underflow_control(dp)) error stop 104_4

!        pass an allocatable of real type argument
         if (ieee_support_underflow_control(ra)) error stop 105_4

!        pass an allocatable of double precision argument
         if (ieee_support_underflow_control(da)) error stop 106_4
   
         nullify(rp,dp)
         deallocate(ra,da)

       end program
