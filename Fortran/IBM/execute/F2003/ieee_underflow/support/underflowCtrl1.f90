! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : underflowCtrl1.f
!*
!*  PROGRAMMER                 : Nancy Wang
!*  DATE                       : Nov. 15 2007
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : ieee_support_underflow_control() 
!*                             : ieee_support_underflow_control(X)
!*  SECONDARY FUNCTIONS TESTED :
!*  REFERENCE                  : Feature Number 289080
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                :    
!*  1. test if ieee_support_underflow_control() returns false without argument
!*  2. test if ieee_support_underflow_control(X) returns false when passing a
!*     real type argument with KIND equal to 4,8,16 respectively
!*  3. test if ieee_support_underflow_control(X) returns false when passing a
!*     pointer of real type argument with KIND equal to 4,8,16 respectively 
!*  4. test if ieee_support_underflow_control(X) returns false when passing an
!*     allocatable of real type argument 
!* 
!23456789012345678901234567890123456789012345678901234567890123456789012   
      module m

         real(4) r1_4
         real(8) r1_8
         real(16) r1_16
         real(4),pointer :: rp1_4=>null()
         real(4),pointer :: rp2_4=>null()
         real(4),target  :: rt1_4
         real(8),pointer :: rp1_8=>null()
         real(8),pointer :: rp2_8=>null()
         real(8),target  :: rt1_8
         real(16),pointer:: rp1_16=>null()
         real(16),pointer:: rp2_16=>null()
         real(16),target :: rt1_16
         real(4),allocatable  :: ra_4
         real(8),allocatable  :: ra_8
         real(16),allocatable :: ra_16

      end module m
      program underflowCtrl1
         use,intrinsic :: ieee_arithmetic
         use m 
         implicit none

         if (.not. associated(rp1_4))  rp1_4=>rt1_4
         if (.not. associated(rp1_8))  rp1_8=>rt1_8
         if (.not. associated(rp1_16)) rp1_16=>rt1_16
         rp2_4=>rp1_4
         rp2_8=>rp1_8
         rp2_16=>rp1_16 
         allocate(ra_4)
         allocate(ra_8)
         allocate(ra_16)

!        pass no argument 
         if (ieee_support_underflow_control()) error stop 101_4 

!        pass a real type argument with KIND=4 
         if (ieee_support_underflow_control(r1_4)) error stop 102_4 

!        pass a real type argument with KIND=8
         if (ieee_support_underflow_control(r1_8)) error stop 103_4 

!        pass a real type argument with KIND=16
         if (ieee_support_underflow_control(r1_16)) error stop 104_4 

!        pass a pointer of real type argument with KIND=4
         if (ieee_support_underflow_control(rp1_4)) error stop 105_4 

!        pass a pointer of real type argument with KIND=4
         if (ieee_support_underflow_control(rp2_4)) error stop 106_4

!        pass a pointer of real type argument with KIND=8
         if (ieee_support_underflow_control(rp1_8)) error stop 107_4

!        pass a pointer of real type argument with KIND=8
         if (ieee_support_underflow_control(rp2_8)) error stop 108_4

!        pass a pointer of real type argument with KIND=16
         if (ieee_support_underflow_control(rp1_16)) error stop 109_4

!        pass a pointer of real type argument with KIND=16
         if (ieee_support_underflow_control(rp2_16)) error stop 110_4

!        pass an allocatable of real type argument with KIND=4
         if (ieee_support_underflow_control(ra_4)) error stop 111_4

!        pass an allocatable of real type argument with KIND=8
         if (ieee_support_underflow_control(ra_8)) error stop 112_4

!        pass an allocatable of real type argument with KIND=16
         if (ieee_support_underflow_control(ra_16)) error stop 113_4

         nullify(rp2_4,rp2_8,rp2_16)
         nullify(rp1_4,rp1_8,rp1_16)
         deallocate(ra_4,ra_8,ra_16) 

       end program
