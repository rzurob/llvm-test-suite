!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParameterInquiryResParam05.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 29 2008 
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
!* 2. TYPE PARAMETER INQUIRY
!* 3. FUNCTION RESULT IS ARRAY WITH TPINQ AS ELEMENTS
!* 4. DEFECT 353790
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    type base(k1,k2,k3,k4,l1,l2)
       integer(1),kind :: k1=kind('c')
       integer(2),kind :: k2=max(1,2)
       integer(kind(5_4)),kind :: k3=k1+k2
       integer(kind(2_8)),kind :: k4=2*k2
       integer(k1),len    :: l1=k1%kind
       integer(k2),len    :: l2=k2%kind

       contains
          procedure,pass :: getParamInfo2
    end type
    contains

     function  getParamInfo2(dt)
        class(base(kind('c'),max(1,2), &
            kind('c')+max(1,2),2*max(1,2),*,*)),intent(in) :: dt
        integer,dimension(12) :: getParamInfo2

        getParamInfo2(1)=dt%k1
        getParamInfo2(2)=dt%k2
        getParamInfo2(3)=dt%k3
        getParamInfo2(4)=dt%k4
        getParamInfo2(5)=dt%l1
        getParamInfo2(6)=dt%l2

        getParamInfo2(7)=dt%k1%kind
        getParamInfo2(8)=dt%k2%kind
        getParamInfo2(9)=dt%k3%kind
        getParamInfo2(10)=dt%k4%kind
        getParamInfo2(11)=dt%l1%kind
        getParamInfo2(12)=dt%l2%kind
     end function

end module

  program dtParameterInquiryResParam05 
  use m
  implicit none

  interface

    function  getParamInfo1(dt)
       import
       type(base(4,1,5,7,*,*)),intent(in) :: dt
       integer,dimension(12) :: getParamInfo1 
    end function
  
  end interface 

  type(base(4,1,5,max(3,7),:,:)),allocatable :: b1
  type(base) :: b2

  !--- defect 353790--!
  allocate(base(kind(2_4),kind('a'), & 
    selected_int_kind(7)+1,max(3,7),99,9) :: b1)

!    allocate(base(4,1,5,7,99,9) :: b1)
    print *,getParamInfo1(b1)
    print *,b2%getParamInfo2()

end

     function  getParamInfo1(dt)
        use m
        type(base(4,1,5,7,*,*)),intent(in) :: dt
        integer,dimension(12) :: getParamInfo1

        getParamInfo1(1)=dt%k1
        getParamInfo1(2)=dt%k2
        getParamInfo1(3)=dt%k3
        getParamInfo1(4)=dt%k4
        getParamInfo1(5)=dt%l1
        getParamInfo1(6)=dt%l2

        getParamInfo1(7)=dt%k1%kind
        getParamInfo1(8)=dt%k2%kind
        getParamInfo1(9)=dt%k3%kind
        getParamInfo1(10)=dt%k4%kind
        getParamInfo1(11)=dt%l1%kind
        getParamInfo1(12)=dt%l2%kind 
        
     end function
