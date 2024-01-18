!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : August 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY
!* 3. GET LENGTH PARAMETER AT RUN TIME, USE READ
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l)
     integer,len  :: l
   end type
end module

  program dtParameterInquiryMis04
  use m
  implicit none

  type(base(:)),allocatable :: b1(:)
  type(base(:)),pointer :: b2(:)
  integer :: length1,bound1,length2,bound2,ios
  character(255) :: errmsg
  integer :: i=0

  do i=1,3
     if(allocated(b1))   deallocate(b1)
     read (*, fmt ='(2i6)',iostat=ios,iomsg=errmsg) length1,bound1
     if(ios /= 0) then
        print *,i,errmsg
        return
     else
        allocate(base(l=length1) :: b1(bound1))
        write (*,fmt='(3i6)') i,b1%l,ubound(b1,1)
     endif
  enddo

  do i=1,3
     read (*, fmt ='(2i6)',iostat=ios,iomsg=errmsg) length2,bound2
     if(ios /= 0) then
        print *,i,errmsg
        return
     else
        allocate(base(l=2*length2) :: b2(2*bound2))
        write (*,fmt='(3i6)') i,b2%l,ubound(b2,1)
     endif
  enddo

end
