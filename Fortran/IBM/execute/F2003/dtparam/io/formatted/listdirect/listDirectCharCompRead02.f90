!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectCharCompRead02.f
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
!* 1. Derived type is polymorphic type and has character array components
!* 2. Test Read statement with list directed input, inputs are delimited character sequence
!234567890123456789012345678901234567890123456789012345678901234567890
module m1
   type base(l1)
      integer,len :: l1 ! l1=3
      character(l1) :: c1(l1:l1+1)
   end type
end module

module m2
use m1
    type,extends(base) :: child(l2)
      integer,len :: l2 ! l2=5
      character(l1+l2) :: c2(l1:l2)
    end type

    contains
      function alloFun(arg)
          class(base(*)),allocatable,intent(in) :: arg(:)
          class(base(:)),pointer :: alloFun(:)

          allocate(alloFun(size(arg,1)),source=arg)

      end function
end module


program listDirectCharCompRead02
  use m2

  interface

     subroutine readData(unit,dummyarg)
       import
       integer,intent(in) :: unit
       class(base(*)),allocatable,intent(in) ::  dummyarg(:)

     end subroutine

  end interface

  integer :: unit=1024,ios
  character(256) :: msg

  class(base(3)),allocatable,target :: base1(:)

  class(base(:)),pointer :: ptr(:)

  allocate(child(3,5) :: base1(3))

  open(unit=unit,file='listDirectCharCompRead02.dat',&
    delim='apostrophe',iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 10
  end if

  call readData(unit,base1)

  ptr(-1:)=>alloFun(base1)

  ! verify result
  select type(ptr)
     type is(child(*,*))
         if(ptr(-1)%c1(3) /= "ABC")         stop 12
         if(ptr(-1)%c1(4) /= "DEF")         stop 13
         if(ptr(-1)%c2(3) /= " 123\,4")     stop 14
         if(ptr(-1)%c2(4) /= "")            stop 15
         if(ptr(-1)%c2(5) /= "/")           stop 16

         if(ptr(0)%c1(3) /= "\'")           stop 17
         if(ptr(0)%c1(4) /= ",;")           stop 18
         if(ptr(0)%c2(3) /= "\"")           stop 19
         if(ptr(0)%c2(4) /= "\'")           stop 20
         if(ptr(0)%c2(5) /= "what")         stop 21

         if(ptr(1)%c1(3) /= "a,")           stop 22
         if(ptr(1)%c1(4) /= "cde")          stop 23
         if(ptr(1)%c2(3) /= "1\"2")         stop 24
         if(ptr(1)%c2(4) /= "3\'4")         stop 25
         if(ptr(1)%c2(5) /= " , ; /")       stop 26
     class default
        stop 27
  end select


  close(unit)

end program

subroutine readData(unit,dummyarg)
   use m2
   implicit none
   integer,intent(in) :: unit
   class(base(*)),intent(inout) :: dummyarg(:)

   select type(x=>dummyarg)
       type is(child(*,*))

           ! read following inputs

           !"ABC" 'DEFG' ' 12
           !3,4', "  ",   '/'
           !"'",',;'  """"  '''' "
           !what"
           !"a, ,b"         , "c
           !d
           !e
           !" , "1""
           !2" '3''
           !4'
           !" , ; /"

           read(unit,*)   x

!           print *,"|",x(1)%c1(3),"|",x(1)%c1(4),"|"
!           print *,"|",x(1)%c2(3),"|",x(1)%c2(4),"|",x(1)%c2(5),"|"
!           print *,"|",x(2)%c1(3),"|",x(2)%c1(4),"|"
!           print *,"|",x(2)%c2(3),"|",x(2)%c2(4),"|",x(2)%c2(5),"|"
!           print *,"|",x(3)%c1(3),"|",x(3)%c1(4),"|"
!           print *,"|",x(3)%c2(3),"|",x(3)%c2(4),"|",x(3)%c2(5),"|"

       class default
          stop 11
   end select

end subroutine
