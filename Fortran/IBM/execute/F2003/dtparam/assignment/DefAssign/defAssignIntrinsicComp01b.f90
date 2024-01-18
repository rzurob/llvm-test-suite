!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignIntrinsicComp01b.f
!*
!*  DATE                       : Feb. 2 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test user defined assignment with generic type bound procedure
!* 2. Arguments have different rank,kind type parameter.
!* 3. Derived type has intrinsic types
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type dtp(k1,l1)
       integer(8),kind :: k1=4
       integer,len     :: l1=3

       character(l1)   :: c1(l1:l1)="******"
       integer(k1)     :: i1(l1:l1+1)=-99
       logical(k1)     :: g1(l1:l1+2)=.false.
       real(k1)        :: r1(l1-2:l1)=-9.9
       contains
         procedure,pass      :: assign1
         procedure,pass(dt1) :: assign2
         procedure,pass      :: assign3
         generic :: assignment ( = ) =>assign1,assign2,assign3
   end type

   contains

       subroutine assign1(dt1,dt2)
           class(dtp(4,*)),intent(inout) :: dt1
           type(dtp(4,*)),intent(in)     :: dt2

           print *,"in assign1"

           dt1%c1=dt2%c1
           dt1%i1=dt2%i1
           dt1%g1=dt2%g1
           dt1%r1=dt2%r1
       end subroutine

       subroutine assign2(dt1,dt2)
           class(dtp(4,*)),intent(inout) :: dt1
           type(dtp(4,3)),intent(in)   :: dt2(:)

           print *,"in assign2"

           dt1%c1="A"//dt2(lbound(dt2,1))%c1(:)(2:2)//"B"
           dt1%i1=dt2(lbound(dt2,1))%i1+100
           dt1%g1=dt2(lbound(dt2,1))%g1 .or. .true.
           dt1%r1=4.5

       end subroutine

       subroutine assign3(dt1,dt2)
           class(dtp(4,*)),intent(inout) :: dt1
           class(dtp(8,3)),intent(in)    :: dt2

           print *,"in aissgn3"

           dt1%c1=dt2%c1
           dt1%i1=dt2%i1
           dt1%g1=dt2%g1
           dt1%r1=dt2%r1

       end subroutine

end module

program defAssignIntrinsicComp01b
     use m
     implicit none

     integer :: i
     type(dtp) :: dtp1

     logical,external :: precision_r4

     type(dtp) :: dtp2=dtp(c1="XLF",i1=[10,11], &
                           g1=[.true.,.false.,.true.],&
                           r1=[1.2,-3.5,6.7E-2])

     type(dtp(4,:)),allocatable :: dtp3(:)

     type(dtp(8,3)),allocatable :: dtp4

     dtp1=dtp2 ! call assign1

     if(any(dtp1%c1 /= "XLF"))                                       stop 10
     if(any(dtp1%i1 /= [10,11]))                                     stop 11
     if(any(dtp1%g1 .neqv. [.true.,.false.,.true.]))                 stop 12
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-2),1.2))        stop 13
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-1),-3.5))       stop 14
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)),6.7E-2))       stop 15

     dtp1=dtp() ! call assign1

     if(any(dtp1%c1 /= "***"))                                       stop 16
     if(any(dtp1%i1 /= -99))                                         stop 17
     if(any(dtp1%g1 .neqv. .false.))                                 stop 18
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-2),-9.9))       stop 19
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-1),-9.9))       stop 20
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)),-9.9))         stop 21

     dtp3=[dtp1,dtp1,dtp1] ! call intrinsic assignment

     if(.not. allocated(dtp3))                                       stop 22
     if(size(dtp3,1) /= 3)                                           stop 23

     dtp1=dtp3 ! call assign2

     if(any(dtp1%c1 /= "A*B"))                                       stop 24
     if(any(dtp1%i1 /= 1))                                           stop 25
     if(any(dtp1%g1 .neqv. .true.))                                  stop 26
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-2),4.5))        stop 27
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-1),4.5))        stop 28
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)),4.5))          stop 29

     dtp4=dtp(8,3)() ! call intrinsic assignment

     dtp1=dtp4 ! call assign3

     if(any(dtp1%c1 /= "***"))                                       stop 30
     if(any(dtp1%i1 /= -99))                                         stop 31
     if(any(dtp1%g1 .neqv. .false.))                                 stop 32
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-2),-9.9))       stop 33
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-1),-9.9))       stop 34
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)),-9.9))         stop 35

end program
