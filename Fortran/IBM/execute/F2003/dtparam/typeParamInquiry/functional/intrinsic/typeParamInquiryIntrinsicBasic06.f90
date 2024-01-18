!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicBasic06.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : July 8 2008  
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
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE 
!* 3. USDED AS PRIMARY IN AN EXPRESSION      
!* 4. USDED AS ACTUAL ARGUMENT 
!* 5. DEFECT 354583 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
    integer(1)    :: i1
    real(4)       :: r1
    complex(8)    :: x1
    logical(1)    :: l1
    character(2)  :: c1='AB'

end module

program typeParamInquiryIntrinsicBasic06
    use m
    implicit none

    integer(2*i1%kind)  :: i2
    complex(x1%kind)    :: x2
    real(max(r1%kind,x1%kind)) :: r2
    logical(4*l1%kind)  :: l2
    character(len=4*(c1%len),kind=c1%kind)  :: c2='' 
    integer :: totalKind
    character(len=*),parameter :: c3="fortran team" 
    character(:),allocatable :: c4(:)
    

    totalKind = getTotalKind( i2%kind,x2%kind,r2%kind,l2%kind,c2%kind)
    if(totalKind /= 23 )                             error stop 10_4 

    call reset(c2,c1%len)
    if(c2 /= 'ABCDEFGH')                             error stop 11_4
    if(c2%len /= 8)                                  error stop 12_4

    allocate(character(c1%len+c2%len+c3%len) :: c4(c1%len+c2%len : c3%len))

    if(c4%len /= len(c4) .or. c4%len /= 22)          error stop 13_4
    if(c4%kind /= kind(c4) .or. c4%kind /= 1)        error stop 14_4
    if(ubound(c4,1) /= 12 .or. lbound(c4,1) /= 10)   error stop 15_4
 
    contains

       integer function getTotalKind (i2_kind,x2_kind,r2_kind,l2_kind,c2_kind)
          integer :: i2_kind,x2_kind,r2_kind,l2_kind,c2_kind

          getTotalKind = i2_kind + x2_kind + r2_kind + l2_kind + c2_kind
       end function


       subroutine reset(c2,len)
          character(*),intent(inout)  :: c2
          integer, intent(in)  :: len

          if( c2%len .ge. len ) then
              c2(1:len)=c1
              c2(len+1:c2%len)='CDEFGH'
          endif 
       end subroutine     
end

