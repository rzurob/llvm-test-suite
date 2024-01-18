! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : underflowCtrl4.f
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
!*  test if ieee_support_underflow_control(X) returns false when passing 
!*  a real data type array,pointer of real type array,allocatable of real 
!*  type array with kind equal to 4,8,16 respectively 
!* 
!23456789012345678901234567890123456789012345678901234567890123456789012   
      module m
         real(4),dimension(2,2)  :: r_4
         real(8),dimension(3,3)  :: r_8
         real(16),dimension(4,4) :: r_16
         real(4),allocatable,dimension(:,:)  :: ra_4
         real(8),allocatable,dimension(:,:)  :: ra_8
         real(16),allocatable,dimension(:,:) :: ra_16
         real(4),pointer,dimension(:,:)  :: rp_4=>null()
         real(8),pointer,dimension(:,:)  :: rp_8=>null()
         real(16),pointer,dimension(:,:) :: rp_16=>null()
         real(4),target,dimension(2,2)   :: rt_4
         real(8),target,dimension(3,3)   :: rt_8
         real(16),target,dimension(4,4)  :: rt_16

      end module m
      program underflowCtrl4
         use,intrinsic :: ieee_arithmetic
         use m 

         if (.not. allocated(ra_4))   allocate(ra_4(2,2)) 
         if (.not. allocated(ra_8))   allocate(ra_8(3,3))
         if (.not. allocated(ra_16))  allocate(ra_16(4,4))
         if (.not. associated(rp_4))  rp_4=>rt_4
         if (.not. associated(rp_8))  rp_8=>rt_8
         if (.not. associated(rp_16)) rp_16=>rt_16 
       
!        pass a real type array argument with KIND=4   
         if (ieee_support_underflow_control(r_4))  error stop 101_4

!        pass a real type array argument with KIND=8  
         if (ieee_support_underflow_control(r_8))  error stop 102_4

!        pass a real type array argument with KIND=16
         if (ieee_support_underflow_control(r_16)) error stop 103_4

!        pass an allocatable of real type array argument with KIND=4
         if (ieee_support_underflow_control(ra_4)) error stop 104_4

!        pass an allocatable of real type array argument with KIND=8 
         if (ieee_support_underflow_control(ra_8)) error stop 105_4

!        pass an allocatable of real type array argument with KIND=16 
         if (ieee_support_underflow_control(ra_16)) error stop 106_4

!        pass a pointer of real type array arguemnt with KIND=4
         if (ieee_support_underflow_control(rp_4)) error stop 107_4

!        pass a pointer of real type array argument with KIND=8
         if (ieee_support_underflow_control(rp_8)) error stop 108_4

!        pass a pointer of real type array argument with KIND=16
         if (ieee_support_underflow_control(rp_16)) error stop 109_4

         if(associated(rp_4))      nullify(rp_4)
         if(associated(rp_8))      nullify(rp_8)
         if(associated(rp_16))     nullify(rp_16) 
         if(allocated(ra_4))       deallocate(ra_4)
         if(allocated(ra_8))       deallocate(ra_8)
         if(allocated(ra_16))      deallocate(ra_16)
          
       end program
