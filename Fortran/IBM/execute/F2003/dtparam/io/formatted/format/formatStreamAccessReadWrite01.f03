!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 12 2008
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. test READ & WRITE statement with stream access
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

program formatStreamAccessReadWrite01
  use m
  implicit none

  integer :: ios,mypos
  character(300) :: msg

  type(Container(:,:)),pointer :: contain1(:)=>null()

  allocate(Container(2,4) :: contain1(2:2))

  contain1=[Container(2,4)(compa=[A(3)("123"),A(3)("456"),A(3)("789")], &
             compb=[B(3)("ABCD"),B(3)("EFGH"),B(3)("IJKL")]) ]

  open(10,file="formatStreamAccessReadWrite01.out",form='formatted', &
        status='new',access='stream',action='write',iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     return
  else
     write(10,'(3a3,3a4)' ) contain1
     inquire(10,pos=mypos)
!     print *,mypos
  end if

  close(10,iostat=ios,status='keep')

  if(ios /= 0)  then
      print *,"error occurred,fail to close the file, iostat=",ios
      return
  end if

  open(11,file="formatStreamAccessReadWrite01.out",form='formatted', &
        status='old',access='stream',action='readwrite',iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     return
  else
     read(11,'(3a3)',pos=11) contain1(2)%compa
     read(11,'(3a4)',pos=3)  contain1(2)%compb
     write(11,'(3a3,3a4)',pos=mypos) contain1
  end if

  close(11,iostat=ios,status='keep')

  if(ios /= 0)  then
      print *,"error occurred,fail to close the file, iostat=",ios
      return
  end if

end program
