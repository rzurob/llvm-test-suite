!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatInternalFile04.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 13 2008 
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
!* 1. test READ & WRITE statement
!* 2. use sequential & stream access
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type buff(size)
      integer,len :: size 
      character(size) :: buffer
  end type

  type dtp(l1)
      integer,len :: l1
      character(l1) :: c1(l1)
  end type 

end module
program formatInternalFile04
  use m
  implicit none

  integer :: ios,i
  character(300) :: msg

  type(buff(:)),allocatable :: buffer1(:)

  type(dtp(:)),allocatable :: dtp1(:)

  dtp1=[dtp(3)(c1=["bcd","efg","hij"]), &
        dtp(3)(c1=["BCD","EFG","HIJ"])]

  allocate(buff(20) :: buffer1(2))
  
  open(10,file='formatInternalFile04.out',form='formatted',pad='no',&
       access='sequential',iomsg=msg,iostat=ios)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 7 
  else
      write(10,'(3a3)') dtp1

      open(10,file='formatInternalFile04.out',form='formatted',pad='yes',&
          access='sequential',iomsg=msg,iostat=ios)
      if(ios /= 0) then
         print *,"fail to open the file"
         print *,"iostat=",ios
         print *,"iomsg=",msg
         stop 8 
      else
          rewind 10
          do i=1,2
            read(10,'(a20)') buffer1(i)  
          end do 

          if(buffer1(1)%buffer /= "bcdefghij")           stop 10
          if(buffer1(2)%buffer /= "BCDEFGHIJ")           stop 11
         
          do i=1,2 
             read(buffer1(i)%buffer,'(3a3)') dtp1(3-i)%c1  
          end do

          if(any(dtp1(1)%c1 /= ["BCD","EFG","HIJ"]))     stop 12
          if(any(dtp1(2)%c1 /= ["bcd","efg","hij"]))     stop 13

      end if

  end if

  close(10,status='delete')

  deallocate(buffer1)
  allocate(buff(20) :: buffer1(2))

  
  open(10,file='formatInternalFile04.out',form='formatted',&   
       access='stream',iomsg=msg,iostat=ios)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 9 
  else
     write(10,'(3a3)') dtp1
     rewind 10
     read(10,'(a9)') buffer1(1)
     read(10,'(a9)',pos=11) buffer1(2)
     read(buffer1(2)%buffer,'(3a3)') dtp1(1)
     read(buffer1(1)%buffer,'(3a3)') dtp1(2)
  end if

  if(buffer1(1)%buffer /= "BCDEFGHIJ")                  stop 14
  if(buffer1(2)%buffer /= "bcdefghij")                  stop 15

  if(any(dtp1(1)%c1 /= ["bcd","efg","hij"]))            stop 16
  if(any(dtp1(2)%c1 /= ["BCD","EFG","HIJ"]))            stop 17

  close(10,status='delete')
    
end program
