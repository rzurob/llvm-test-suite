!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatStreamAccessBasicRead01.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 11 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   :  
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!* 1. test READ statement with stream access
!* 2. derived type has 2 layers of nested component which has sequence property
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner1(l1)
      integer,len :: l1  ! l1=4
      sequence
      character(l1+1) :: c1
      integer      :: i1(l1:l1+1) 
   end type

   type inner2(l2)
      integer,len :: l2 ! l2=3
      sequence
      logical     :: log1(l2)
      type(inner1(l2+1)) :: inn1
   end type
   
   type outer(l3)
      integer,len   :: l3  ! l3=4
      sequence
      character(l3) :: c2(l3)
      type(inner2(l3-1)) :: inn2
   end type 

end module

program formatStreamAccessBasicRead01
  use m
  implicit none

  type(outer(:)),pointer :: pouter=>null()
  type(outer(4)),target  :: tar

  integer :: ios,mypos
  character(300) :: msg

  pouter=>tar

  open(10,file="formatStreamAccessBasicRead01.in",form="formatted", &
       action='read',access='stream',iostat=ios,iomsg=msg)


  if(ios /= 0) then
      print *,"fail to open the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 11
  else
      inquire(file="formatStreamAccessBasicRead01.in",pos=mypos)
      read(10,fmt='(4a3)',pos=mypos) tar%c2
      read(10,fmt='(l5)',pos=19) tar%inn2%log1(2)
      read(10,fmt='(l1)',pos=10) tar%inn2%log1(1)
      read(10,fmt='(l3)',pos=28) tar%inn2%log1(3)
      read(10,fmt='(a2)',pos=26) tar%inn2%inn1%c1
      read(10,fmt='(i1)',pos=31) tar%inn2%inn1%i1(ubound(tar%inn2%inn1%i1,1))
      read(10,fmt='(i2)',pos=30) tar%inn2%inn1%i1(lbound(tar%inn2%inn1%i1,1))

      write(*,'(4a4,/3l2,/a5,/2i2)') pouter 
      
      read(10,'(4a4,/3l2,/a5,2i1)',pos=1 )  tar

      write(*,'(4a4,/3l3,/a5,/2i2)') pouter 
  
  end if   

  close(10)
  
end program
