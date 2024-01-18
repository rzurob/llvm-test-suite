!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : formatDirectAccessReadWrite01.f   
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
!* 1. READ or WRITE from or to the same file with direct access 
!* 2. test rewrite records
!* 3. derived type has nested derived type component
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type inner(l1)
      integer,len :: l1
      integer :: i1(l1:l1)  ! l1=3
      logical :: log1(l1:l1+1)
   end type

   type outer(l2)
      integer,len :: l2
      integer     :: i2(l2) ! l2=2
      type(inner(l2+1)) :: comp
   end type
end module

program formatDirectAccessReadWrite01
  use m
  implicit none

  integer :: ios,i
  integer,parameter :: len=3
  character(300) :: msg

  type(outer(:)),pointer :: outer1(:)=>null()

  type(outer(2)),target  :: tar1(1:len),tar2(len+1:2*len)

  outer1=>tar2

  tar1=[outer(2)(i2=[11,12],comp=(inner(3)(i1=13,log1=[.true.,.false.]))),&
          outer(2)(i2=[21,22],comp=(inner(3)(i1=23,log1=[.false.,.true.]))),&
          outer(2)(i2=[31,32],comp=(inner(3)(i1=33,log1=[.true.,.true.])))]

  open(10,file='formatDirectAccessReadWrite01.out',form='formatted',&
          status='new',action='readwrite',access='direct',recl=32, &
          iostat=ios,iomsg=msg)

  if(ios /= 0) then
     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop  10
  else
     write(10,'(a)',rec=1) "first record"

     ! write tar1(1),tar1(2),tar1(3) to record 5,6,7
     write(10,'(3i3,2l3/3i3,2l3/3i3,2l3)',rec=5) (tar1(i),i=1,3)

     !swap record 7 & record 5
     write(10,'(3i3,2l3)',rec=7) tar1(1)
     write(10,'(3i3,2l3)',rec=5) tar1(len)

     ! read record into tar2
     read(10,'(3(3i3,2l3,:,/))',rec=5) tar2

     if(any(outer1(4)%i2 /= [31,32]))                     stop 11
     if(any(outer1(4)%comp%i1/= [33]))                    stop 12
     if(any(outer1(4)%comp%log1 .neqv. [.true.,.true.]))  stop 13

     if(any(outer1(5)%i2 /= [21,22]))                     stop 14 
     if(any(outer1(5)%comp%i1/= [23]))                    stop 15
     if(any(outer1(5)%comp%log1 .neqv. [.false.,.true.])) stop 16

     if(any(outer1(6)%i2 /= [11,12]))                     stop 17
     if(any(outer1(6)%comp%i1/= [13]))                    stop 18
     if(any(outer1(6)%comp%log1 .neqv. [.true.,.false.])) stop 19

     ! rewrite the record 5,6,7
     write(10,'(3(3i3,2l3,:,/))',rec=5) tar2(2*len:len+1:-1)
     read(10,'(3(3i3,2l3,:,/))',rec=5) tar1(len:1:-1)

     if(any(tar1(1)%i2 /= [31,32]))                       stop 20
     if(any(tar1(1)%comp%i1/= [33]))                      stop 21
     if(any(tar1(1)%comp%log1 .neqv. [.true.,.true.]))    stop 22

     if(any(tar1(2)%i2 /= [21,22]))                       stop 23
     if(any(tar1(2)%comp%i1/= [23]))                      stop 24
     if(any(tar1(2)%comp%log1 .neqv. [.false.,.true.]))   stop 25

     if(any(tar1(3)%i2 /= [11,12]))                       stop 26
     if(any(tar1(3)%comp%i1/= [13]))                      stop 27
     if(any(tar1(3)%comp%log1 .neqv. [.true.,.false.]))   stop 28    

     do i=lbound(tar2,1),ubound(tar2,1)
        tar2(i)%i2=-1*tar2(i)%i2
        tar2(i)%comp%i1=-1*tar2(i)%comp%i1 
     end do 

     outer1=>tar1
     
     ! rewrite record 5,6,7
     write(10,'(3(3i3,2l3,:/))',rec=5) tar2

     read(10,'(3(3i3,2l3,:,/))',rec=5) outer1

     if(any(tar1(1)%i2 /= [-31,-32]))                     stop 29
     if(any(tar1(1)%comp%i1/= [-33]))                     stop 30 

     if(any(tar1(2)%i2 /= [-21,-22]))                     stop 31 
     if(any(tar1(2)%comp%i1/= [-23]))                     stop 32 

     if(any(tar1(3)%i2 /= [-11,-12]))                     stop 33
     if(any(tar1(3)%comp%i1/= [-13]))                     stop 34
      
  end if  

  close(10)

end program
