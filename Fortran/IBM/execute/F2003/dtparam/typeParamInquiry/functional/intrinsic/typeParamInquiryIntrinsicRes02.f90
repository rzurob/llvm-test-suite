!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : July 30 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. FUNCTION RESULT IS TYPE PARAMETER INQUIRY
!* 3. USE FUNCTION RESULT AS ARRAY BOUND
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicRes02
    implicit none

    integer(selected_int_kind(2)) :: i1=selected_int_kind(2)
    integer(4),allocatable :: i2(:)
    character(len=*),parameter :: c1='xlftest'
    character(len=10),target :: c2='fortran'
    character(:),pointer :: c3(:,:)
    character(:),allocatable :: c4(:)

    allocate(i2(getkind1(i1)),source=getkind1(i1))
    if(ubound(i2,1) /= 2)                           error stop 10_4
    if(any(i2 /= 2))                                error stop 11_4

    allocate( c3(getlen1(c1),getlen1(c2)) , &
      source= char(getlen1(c1//c2)) )
    if(ubound(c3,1) /= 14)                          error stop 12_4
    if(ubound(c3,2) /= 20)                          error stop 13_4
    if(lbound(c3,1) /= 1)                           error stop 14_4
    if(lbound(c3,1) /= 1)                           error stop 15_4
    if(any(c3 /= char(getlen1(c1//c2)) ))           error stop 16_4


    allocate( c4(getlen1(c2):getlen1(c1)) , &
              source= char(getlen1('abc')) )

    if(lbound(c4,1) /= 1)                           error stop 17_4
    if(ubound(c4,1) /= 0)                           error stop 18_4
    if(any(c4 /= char(getlen1('abc')) ))            error stop 19_4
    contains
       integer function getkind1(arg)
          integer(selected_int_kind(2)) :: arg
          getkind1=arg%kind+kind(arg)
       end function

       integer function getlen1(arg)
          character(*),intent(in) :: arg
          getlen1=arg%len+len(arg)
       end function

end

