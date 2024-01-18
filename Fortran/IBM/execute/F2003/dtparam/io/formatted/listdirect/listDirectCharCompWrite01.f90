!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectCharCompWrite01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Jan. 7 2009 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!* 1. Derived type has ultimate character components
!* 2. Test write statement with different value of delimiters: APOSTROPHE, QUOTE,NONE 
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
  type inner(l2)
     integer,len :: l2  ! l2=6
     character(l2) :: c2(l2)
     character(l2+1) :: c3 
  end type
end module

module m2
use m1
   type outer(l1)
      integer(8),len :: l1 ! l1=5
      character(l1+1) :: c1(l1)
      type(inner(l1+1)) :: comp 
   end type 
end module

subroutine writeDT1(dtArg)
  use m2
  implicit none
  type(outer(*)),intent(in) :: dtArg(:)
  integer :: ios
  character(256) :: msg

  ! page 242: character sequences produced when the delimiter mode has a value of APOSTROPHE are delimited by apostrophes, are preceded and followed by a value separator , and have each internal apostrophe represented on the external medium by TWO contiguous apostrophes.

  write(*,fmt=*,delim='apostrophe',iostat=ios,iomsg=msg) dtArg
  if(ios <> 0)   then
     print *,"error in writing data"
     print *,"iostate=",ios
     print *,"iomsg=",msg
     stop 11
  end if 

  ! page 242: character sequences produced when the delimiter mode has a value of QUOTE are delimited by quotes, are preceded and followed by a value separater, and have each internal quote represented on the external medium by TWO contiguous quotes.

  write(*,fmt=*,delim='quote',iostat=ios,iomsg=msg) dtArg
  if(ios <> 0)   then
     print *,"error in writing data"
     print *,"iostate=",ios
     print *,"iomsg=",msg
     stop 12
  end if
  ! page 242: Character sequences produced when the delimiter mode has a value of NONE:
  ! 1) Are not delimited by apostrophes or quotation marks
  ! 2) Are not separated from each other by value separators
  ! 3) Have each internal apostrophe or quotation mark represented externally by one apostrophe or quotation mark


  write(*,fmt=*,delim='none',iostat=ios,iomsg=msg) dtArg
  if(ios <> 0)   then
     print *,"error in writing data"
     print *,"iostate=",ios
     print *,"iomsg=",msg
     stop 13
  end if

end subroutine


subroutine writeDT2(dtArg)
  use m2
  implicit none
  type(outer(*)),intent(in) :: dtArg(:)
  integer :: ios,i
  character(256) :: msg

  do i=lbound(dtArg,1),ubound(dtArg,1)
    write(*,fmt=*,delim='apostrophe',iostat=ios,iomsg=msg)  &
       dtArg(i)%c1,dtArg(i)%comp%c2,dtArg(i)%comp%c3
    if(ios <> 0)   then
       print *,"error in writing data"
       print *,"iostate=",ios
       print *,"iomsg=",msg
       stop 14
    end if

    write(*,fmt=*,delim='quote',iostat=ios,iomsg=msg)  &
       dtArg(i)%c1,dtArg(i)%comp
    if(ios <> 0)   then
       print *,"error in writing data"
       print *,"iostate=",ios
       print *,"iomsg=",msg
       stop 15
    end if

    write(*,fmt=*,delim='none',iostat=ios,iomsg=msg)  &
       dtArg(i)
    if(ios <> 0)   then
       print *,"error in writing data"
       print *,"iostate=",ios
       print *,"iomsg=",msg
       stop 16
    end if
  end do

end subroutine

program listDirectCharCompWrite01
  use m2
  implicit none

  interface 
    subroutine writeDT1(dtArg)
       import 
       type(outer(*)),intent(in) :: dtArg(:)
    end subroutine

    subroutine writeDT2(dtArg)
       import
       type(outer(*)),intent(in) :: dtArg(:)
    end subroutine

  end interface 

  integer,parameter :: N=1
  integer :: i,ios
  character(256) :: msg

  type(outer(5)),target :: tar1(N)
  type(outer(:)),pointer :: ptr1(:)=>null()

  do i=1,N
    tar1(i)%c1="12345/"
    tar1(i)%comp%c2="a\"b\"d:"
    tar1(i)%comp%c3="\'isn\'t\'"
  end do

  ptr1(2:)=>tar1

  call writeDT1(ptr1)

  call writeDT2(tar1) 

end program
