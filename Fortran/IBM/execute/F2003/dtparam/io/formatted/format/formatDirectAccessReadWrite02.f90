!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatDirectAccessReadWrite02.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Dec. 15 2008 
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
!*  1. READ or WRITE from or to same file with direct access
!*  2. derived type is polymorphic type 
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1) 
      integer,len :: l1
      character(l1+1) :: c1 !l1=2
   end type

   type,extends(base) :: child(l2)
      integer,len :: l2
      character(l1+l2) :: c2(l1:l2) !l2=3
   end type

   type,extends(child) :: gen3(l3)
     integer,len :: l3   ! l3=4
     character(l1+l2+l3) :: c3(l3:l1+l2)
     type(child(l3-l1,l3-1)) :: comp 
   end type

end module

program formatDirectAccessReadWrite02
  use m
  interface
     subroutine readData1(unit,dtp)
        import 
        class(base(*)),intent(inout) :: dtp
        integer,intent(in) :: unit

     end subroutine
     subroutine writeData1(unit,dtp)
        import
        class(base(*)),intent(inout) :: dtp
        integer,intent(in) :: unit

     end subroutine

  end interface

  implicit none

  class(base(2)),pointer :: base1(:)=>null()
  class(base(:)),pointer :: base2(:)=>null()
  type(gen3(2,3,4)),target :: tar1(2:3)
 
 
  integer :: ios
  character(300) :: msg

  tar1(2)%c1="IBM"
  tar1(2)%c2=["TELUS","INTEL"]
  tar1(2)%c3=["XLFORTRAN","TEST TEAM"]
  tar1(2)%comp=child(2,3)(c1="123",c2=["abcde","ABCDE"])
 
  base1(-1:)=>tar1(2:3)
  allocate(gen3(2,3,4) :: base2(2:3))

  open(10,file='formatDirectAccessReadWrite02.out',form='formatted',&
         access='direct',action='readwrite',status='new',&
         recl=48,iostat=ios,iomsg=msg)

  if(ios /= 0) then
    print *,"fail to open the file"
    print *,"iostat=",ios
    print *,"iomsg=",msg
    return
  end if

   associate(x=>tar1)
     write(10,'(a3)',rec=15) x(2)%c1
     write(10,'(a5,a5)',rec=100) x(2)%c2
     write(10,'(2a9)',rec=99) x(2)%c3
     write(10,'(a3,2a5)',rec=101) x(2)%comp 

     call readData1(10,x(3))

     x(3)%c1="RED"
     x(3)%c2=["LIGHT","HIGHT"]
     x(3)%c3=["TELEPHONE","MICROWAVE"]
     x(3)%comp=child(2,3)(c1="456",c2=["hijkl","HIJKL"])
          
     call writeData1(10,x(3))
   end associate

  select type(base1)
      type is(gen3(*,*,*))
        write(10,'(a3,2a5,2a9,a3,2a5)',rec=5) base1
      class default
        stop 13
  end select

  select type(base2)
      type is(gen3(*,*,*))
        read(10,'(a3,2a5,2a9,a3,2a5)',rec=5) base2
        write(*,'(a3,2a5,2a9,a3,2a5)') base2
      class default
        stop 14
  end select
         
  close(10)

end program

subroutine readData1(unit,dtp)
  use m
  class(base(*)),intent(inout) :: dtp
  integer,intent(in) :: unit

  select type(dtp)
     type is(gen3(*,*,*))

       read(unit,'(a3)',rec=15) dtp%c1
       read(unit,'(a5,a5)',rec=100) dtp%c2
       read(unit,'(2a9)',rec=99) dtp%c3
       read(unit,'(a3,2a5)',rec=101) dtp%comp

       write(*,'(a3/2a5/2a9/a3,2a5,:/)') dtp
  
     class default
        stop 11
  end select      
end subroutine

subroutine writeData1(unit,dtp)
  use m
  class(base(*)),intent(inout) :: dtp
  integer,intent(in) :: unit

  select type(dtp)
     type is(gen3(*,*,*))

       write(unit,'(a3)',rec=15) dtp%c1
       write(unit,'(a5,a5)',rec=100) dtp%c2
       write(unit,'(2a9)',rec=99) dtp%c3
       write(unit,'(a3,2a5)',rec=101) dtp%comp

       write(*,'(a3/2a5/2a9/a3,2a5,:/)') dtp

     class default
        stop 12
  end select
end subroutine

