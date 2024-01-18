!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatInternalFile02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 10 2008 
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
!*  1. test READ & WRITE in internal file(string buffer)
!*  2. read the sequential records into buffer, and read buffer out into derived type
!*  3. derived type is polymorphic type.
!*  4. internal file is allocatable default character array with deferred length parameter
!234567890123456789012345678901234567890123456789012345678901234567890
module m
  type base(k1,l1)
     integer,kind  :: k1 !k1=2
     integer,len   :: l1 !l1=3
     integer(k1)   :: i(l1-1) 
  end type

  type,extends(base) :: child(l2)
     integer,len   :: l2
     character(l2) :: c(l2) !l2=3
     logical(k1)   :: log(l2)
     real(k1+k1)   :: r(l2)
  end type

end module

program formatInternalFile02
  use m
  implicit none

  class(base(2,:)),allocatable :: base1,base2
  character(:),allocatable :: buffer(:)
  integer :: ios,i

  allocate(child(2,3,3) :: base1,base2)
  allocate(character(60) :: buffer(0:3) )

  select type(base1)
    type is(child(2,*,*))
       base1%i=[10,-20]
       base1%c=["xlf","ibm","xlc"]
       base1%log=[.true.,.false.,.true.]
       base1%r=[2.34,1.345E+02,-99.12]
    class default
       stop 10
  end select

  open(10,file="formatInternalFile02.dat",form='formatted',&
       access='sequential',sign='suppress',iostat=ios)

  if(ios /= 0) then
    print *,"fail to open the file, iostat=",ios 
    stop 11 
  else
     select type(base1)
        type is(child(2,*,*))
            write(10,'(sp,2i4,/3a3/3l3/f5.2,e12.3,f7.2)') base1 
        class default
           stop 12
     end select
  end if

  rewind(10)
 
  read(10,'(a30/a30/a30/a30)')  (buffer(i),i=0,3)

  do i=0,3
     write(*,'(a30)') buffer(i) 
  end do

  select type(base2)
     type is(child(2,*,*))
         read(buffer,'(2i4/3a3/3l3/f5.2,e12.3,f7.2)') base2

         write(*,'(sp,2i4,/3a3/3l3/f5.2,e12.3,f7.2)') base2
     class default
         stop 13
  end select

  close(10) 

end program
