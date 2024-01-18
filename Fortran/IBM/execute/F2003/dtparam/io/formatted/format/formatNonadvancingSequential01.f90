!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 18 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : FORMATTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. test Write & Read for non-advancing IO
!*  2. derived type is polymorphic type
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type base(l1,l2)
     integer,len :: l1,l2 ! l1=3,l2=4
     character(l1+l2) :: c1(l1:l2)
   end type

   type,extends(base) :: child(l3)
     integer,len :: l3 ! l3=2
     integer     :: i1(l3)
     character(l3) :: c2
     real        :: r1(l3)
   end type
end module

program formatNonadvancingSequential01
  use m
  implicit none

  integer :: ios,count,errnum=25
  character(300) :: msg,tmpchar

  class(base(3,4)),pointer :: poly1=>null(),poly2=>null()

  allocate(child(3,4,2) ::  poly1,poly2)

  select type(poly1)
     type is(child(*,*,*))
        poly1%c1=["FORTRAN","TORONTO"]
        poly1%i1=[11,-12]
        poly1%c2="XLF"
        poly1%r1=[12.34,-5.67]

     class default
        stop 10

  end select

  open(10,file='formatNonadvancingSequential01.out',form='formatted',pad='yes',&
     action='readwrite',status='replace',access='sequential',position='rewind',&
       recl=1000,iostat=ios,iomsg=msg)

  if(ios /= 0) then
    print *,"error in opening the file"
    print *,"iostat=",ios
    print *,"iomsg=",msg
    stop 9
  end if

  select type(poly1)
     type is(child(*,*,*))

       ! start first record
       write(10,fmt='(a7)',advance='no') poly1%c1(4)

       ! position file before the current record
       backspace(10)

       ! rewrite first record
       write(10,fmt='(2a7)',advance='no') poly1%c1

       ! second record
       write(10,fmt='(/sp,2i4,a4/)',advance='no') poly1%i1,poly1%c2

       ! third record
       write(10,fmt='(2f6.2)',advance='no') poly1%r1

       rewind 10

       select type(poly2)
          type is(child(*,*,*))

            ! read c1(3) in first record
            read(10,'(a4)',eor=12,size=count,advance='no') poly2%c1(3)
            if(count /= 4)       stop 13

            ! skip 6 characters in first record
            read(10,'(a6)',eor=14,size=count,advance='no') tmpchar
            if(count /= 6)       stop 15

            ! read c1(4) in first record
            read(10,'(a4/)',eor=16,size=count,advance='no') poly2%c1(4)
            if(count /= 4)       stop 17

            ! skip first 3 characters in second record
            read(10,'(a3)',eor=18,size=count,advance='no') tmpchar
            if(count /= 3)       stop 19

            ! read i1(1) in second record
            read(10,'(i1)',eor=20,size=count,advance='no') poly2%i1(1)
            if(count /= 1)        stop 21

            ! read i1(2) in second record
            read(10,'(i3)',eor=22,size=count,advance='no') poly2%i1(2)
            if( count /= 3)      stop 23

            ! skip those 3 character
            read(10,'(a3)',eor=24,size=count,advance='no') tmpchar
            if(count /= 3)       stop 25

            ! read third record
            read(10,'(a2/)',eor=26,size=count,advance='no') poly2%c2
            if(count /= 2)       stop 27

            ! read fourth record
            read(10,'(2f6.2)',eor=28,size=count,advance='no') poly2%r1
            if(count /= 12)       stop 29

            write(10,fmt='(/a)',advance='no') "Value of Poly2:"

            ! write all in 1 record
           write(10,fmt='(/2a7,sp,2i4,a4,2f6.2)',advance='no')  poly2

           ! write an end of file record
           endfile(10)

          class default
            stop 13
       end select

     class default

       stop 11
  end select


  close(10,iostat=ios,status='keep')

  if(ios /= 0) then
     print *,"error in closing the file,iostat=",ios
     stop 30
  end if

  return

12 print *,"error in reading file,end of record reached,iostat=",ios
   stop 12
14 print *,"error in reading file,end of record reached,iostat=",ios
   stop 14
16 print *,"error in reading file,end of record reached,iostat=",ios
   stop 16
18 print *,"error in reading file,end of record reached,iostat=",ios
   stop 18
20 print *,"error in reading file,end of record reached,iostat=",ios
   stop 20
22 print *,"error in reading file,end of record reached,iostat=",ios
   stop 22
24 print *,"error in reading file,end of record reached,iostat=",ios
   stop 24
26 print *,"error in reading file,end of record reached,iostat=",ios
   stop 26
28 print *,"error in reading file,end of record reached,iostat=",ios
   stop 28

end program
