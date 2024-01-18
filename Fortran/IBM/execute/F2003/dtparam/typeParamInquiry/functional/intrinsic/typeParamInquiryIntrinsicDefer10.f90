!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : typeParamInquiryIntrinsicDefer10.f
!*
!*  DATE                       : August 5 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : TYPE PARAMETER INQUIRY
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* 1. TEST SECTION 6.1.3
!* 2. TYPE PARAMETER INQUIRY FOR INTRINSIC TYPE
!* 3. DEFERRED TYPE PARAMETER INQUIRY
!* 4. ALLOCATABLE AND POINTER
!* 5. LENGTH OF CHARACTER IS SPECIFIED AT RUN TIME
!*    READ LENGTH FROM STD INPUT
!234567890123456789012345678901234567890123456789012345678901234567890

program typeParamInquiryIntrinsicDefer10
    implicit none

    character(:),allocatable :: a(:)
    character(:),pointer :: b(:)

    integer  :: length,i
    integer  :: stat
    character(256)  :: msg

    do i=1, 3
       read(* ,fmt='(i2)',iostat=stat,iomsg=msg)  length
       if(stat /= 0) then
          print *,msg
          return
       else
          if(allocated(a))  deallocate(a)
          allocate(character(length) :: a(length))

       endif
       print *,a%len,len(a)
       print *,lbound(a,1),ubound(a,1)
       print *,a%kind,kind(a)
    end do

    do i=1, 3
       read(* ,fmt='(i2)',iostat=stat,iomsg=msg)  length
       if(stat /= 0) then
          print *,msg
          return
       else
          allocate(character(2*length) :: b(2*length))

       endif
       print *,b%len,len(b)
       print *,lbound(b,1),ubound(b,1)
       print *,b%kind,kind(b)
       deallocate(b)

    end do
end

