!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 11 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. test Write statement with stream access
!*  2. derived type has 2 layers of nested derived type component
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner1(l1)
      integer,len :: l1
      character(l1+1) :: c1
      integer      :: i1(l1:l1+1)
   end type

   type inner2(l2)
      integer,len :: l2
      logical     :: log1(l2)
      type(inner1(l2+1)) :: inn1
   end type

   type outer(l3)
      integer,len   :: l3
      character(l3) :: c2(l3)
      type(inner2(l3-1)) :: inn2
   end type

end module

program formatStreamAccessBasicWrite01
  use m
  implicit none

  type(outer(:)),pointer :: pouter=>null()
  type(outer(4)),target  :: tar

  integer :: ios,mypos
  character(300) :: msg

  tar%c2=["abcd","ABCD","efgh","EFGH"]
  tar%inn2%log1=[.true.,.false.,.true.]
  tar%inn2%inn1%c1="fortran"
  tar%inn2%inn1%i1=[1,2]

  pouter=>tar

  open(10,file="formatStreamAccessBasicWrite01.out",form="formatted", &
       access='stream',iostat=ios,iomsg=msg)


  if(ios /= 0) then
      print *,"fail to open the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 11
  else
      inquire(10,pos=mypos)
      if(mypos /= 1)  stop 12

      write(10,'(4(a4))',pos=mypos) pouter%c2
      inquire(10,pos=mypos)
      if(mypos /= 18) stop 13

      write(10,'(3l2)',pos=mypos) pouter%inn2%log1
      inquire(10,pos=mypos)
      if(mypos /= 25) stop 14

      write(10,'(a5,2i1)',pos=mypos) pouter%inn2%inn1
      inquire(10,pos=mypos)
      if(mypos /= 33) stop 15

      write(10,'(4a4,3l2,a5,2i1)',pos=mypos) pouter
      inquire(10,pos=mypos)
      if(mypos /= 63) stop 16

  end if

  close(10)

end program
