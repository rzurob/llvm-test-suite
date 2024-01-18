!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatStreamAccessReadWrite02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 12 2008 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!*  1. file is opened and written by using stream access method and is closed, reopened by sequntial access method, and read records and write new record with sequential access method
!*  2. read & write in same file
!*  3. derived type has 2 derived type components and both have sequence property
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type A(l1)
     integer,len :: l1
     sequence
     character(l1) :: c1 !l1=3
   end type
   
   type B(l2)
     integer,len :: l2
     sequence
     character(l2+len("A")) :: c2 ! l2=3
   end type 
   
   type Container(l3,l4)
      integer,len :: l3,l4
      type(A(l3+1)) :: compa(l3:l4) !l3=2, l4=4
      type(B(l4-1)) :: compb(l3:l4)
   end type    
   
end module

program formatStreamAccessReadWrite02
  use m
  implicit none

  integer :: ios
  character(300) :: msg

  type(Container(:,:)),pointer :: contain1(:)=>null()

  allocate(Container(2,4) :: contain1(2:2))

  contain1=[Container(2,4)(compa=[A(3)("123"),A(3)("456"),A(3)("789")], &
             compb=[B(3)("ABCD"),B(3)("EFGH"),B(3)("IJKL")]) ]

  open(10,file="formatStreamAccessReadWrite02.out",form='formatted', &
        status='new',access='stream',action='write',iostat=ios,iomsg=msg) 

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 11 
  else
     write(10,'(3a3,3a4)' ) contain1
  end if

  close(10,iostat=ios,status='keep')

  if(ios /= 0)  then
      print *,"error occurred,fail to close the file, iostat=",ios 
      stop 12 
  end if

  open(11,file="formatStreamAccessReadWrite02.out",form='formatted', &
       status='old',access='sequential',action='readwrite',iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 13 
  else
     read(11,'(t11,3a3)') contain1(2)%compa
     rewind(11)
     read(11,'(t3,3a4)')  contain1(2)%compb
     write(11,'(3a3,3a4)') contain1
  end if

  close(11,iostat=ios,status='keep')

  if(ios /= 0)  then
      print *,"error occurred,fail to close the file, iostat=",ios
      stop 14 
  end if

end program
