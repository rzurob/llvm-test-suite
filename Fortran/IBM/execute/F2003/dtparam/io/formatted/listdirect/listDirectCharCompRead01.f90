!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 12 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. Derived type has ultimate scalar character components
!*  2. Test Read statement,the first nonblank character is not quotation mark or apostrophe
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
  type TCHAR1(l1)
     integer,len     :: l1
     character(l1)   :: ch1
     character(l1+1) :: ch2
  end type
end module

module m2
use m1
  type TCHAR2(l2)
     integer,len   :: l2
     character(l2) :: ch3
     type(TCHAR1(l2+1))  :: tch1
  end type
end module


program listDirectCharCompRead01
  use m2
  implicit none

  integer :: ios,i
  character(256) :: msg

  type(TCHAR2(:)),allocatable :: t2(:)

  allocate(TCHAR2(5) :: t2(2))

  open(unit=10,file='listDirectCharCompRead01.dat',status='old',delim='apostrophe',iostat=ios,iomsg=msg)

  if( ios <> 0) then
     write(*,*) "fail to open the file"
     write(*,*) "iostat=",ios
     write(*,*) "iosmsg=",msg
     stop 10
  end if

  ! read first line, it has blank separator,blanks are 1 or more space or tab
  read(10,*) t2

  if(t2(1)%ch3 /= "We")            stop 11
  if(t2(1)%tch1%ch1 /= "are")      stop 12
  if(t2(1)%tch1%ch2 /= "going")    stop 13
  if(t2(2)%ch3 /= "to")            stop 14
  if(t2(2)%tch1%ch1 /= "have")     stop 15
  if(t2(2)%tch1%ch2 /= "dinner")   stop 16

  deallocate(t2)
  allocate(TCHAR2(5) :: t2(2))

  ! read second line, it has comma and slash separator
  read(10,*) t2

  if(t2(1)%ch3 /= "We")            stop 17
  if(t2(1)%tch1%ch1 /= "are")      stop 18
  if(t2(1)%tch1%ch2 /= "going")    stop 19
  if(t2(2)%ch3 /= "to")            stop 20
  if(t2(2)%tch1%ch1 /= "have")     stop 21
  if(t2(2)%tch1%ch2 /= "dinner")   stop 22

!  write(*,*) "|",t2(1)%ch3,"|",t2(1)%tch1%ch1,"|",t2(1)%tch1%ch2,"|"
!  write(*,*) "|",t2(2)%ch3,"|",t2(2)%tch1%ch1,"|",t2(2)%tch1%ch2,"|"

  deallocate(t2)

  allocate(TCHAR2(3) :: t2(-1:1))

  ! set value for t2(1)
  t2(1)%ch3="123"
  t2(1)%tch1%ch1="456"
  t2(1)%tch1%ch2="789"

  ! read third line,separator are blank, comma, slash
  read(10,*) t2

  if(t2(-1)%ch3 /= "x**")          stop 23
  if(t2(-1)%tch1%ch1 /= "un_d")    stop 24
  if(t2(-1)%tch1%ch2 /= "longw")   stop 25
  if(t2(0)%ch3 /= "a")             stop 26
  if(t2(0)%tch1%ch1 /= "25")       stop 27
  if(t2(0)%tch1%ch2 /= "*")        stop 28
  if(t2(1)%ch3 /= "I!s")           stop 29

  ! t2(1)%tch1%ch1 & t2(1)%tch1%ch2 are not changed since input value are determinated with slash separator after read t2(1)%ch3

  if(t2(1)%tch1%ch1 /= "456")      stop 30
  if(t2(1)%tch1%ch2 /= "789")      stop 31

 ! write(*,*) "|",t2(-1)%ch3,"|",t2(-1)%tch1%ch1,"|",t2(-1)%tch1%ch2,"|"
 ! write(*,*) "|",t2(0)%ch3,"|",t2(0)%tch1%ch1,"|",t2(0)%tch1%ch2,"|"
 ! write(*,*) "|",t2(1)%ch3,"|",t2(1)%tch1%ch1,"|",t2(1)%tch1%ch2,"|"

  deallocate(t2)

  allocate(TCHAR2(5) :: t2(2))

  ! read fourth, fifth, sixth lines,delimiters are within character sequence.
  read(10,*) t2

  if(t2(1)%ch3 /= "a\"\"bc")           stop 32
  if(t2(1)%tch1%ch1 /= "d\'e\'f\'")    stop 33
  if(t2(1)%tch1%ch2 /= "g\"\"\"")      stop 34
  if(t2(2)%ch3 /= "he\"\'\'")          stop 35
  if(t2(2)%tch1%ch1 /= "!\'\'\'\'")    stop 36
  if(t2(2)%tch1%ch2 /= "mo\"rn\"")     stop 37

!  write(*,*) "|",t2(1)%ch3,"|",t2(1)%tch1%ch1,"|",t2(1)%tch1%ch2,"|"
!  write(*,*) "|",t2(2)%ch3,"|",t2(2)%tch1%ch1,"|",t2(2)%tch1%ch2,"|"

  close(10)

end program
