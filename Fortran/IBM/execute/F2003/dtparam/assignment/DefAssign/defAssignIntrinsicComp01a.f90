!*********************************************************************
!*  ===================================================================
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
!* 1. Test user defined assignment with interface block
!* 2. Arguments have different rank , kind type parameter.
!* 3. Derived type has intrinsic types
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type dtp(k1,l1)
       integer(8),kind :: k1=4
       integer,len     :: l1=3

       character(l1)    :: c1(l1:l1)="******"
       integer(k1)      :: i1(l1:l1+1)=-99
       logical(k1)      :: g1(l1:l1+2)=.false.
       real(k1)         :: r1(l1-2:l1)=-9.9
   end type

   interface assignment( = )

       module procedure assign1
       module procedure assign2
       module procedure assign3
       module procedure assign4
       module procedure assign5

   end interface

   contains

       subroutine assign1(dt1,dt2)
           type(dtp(4,*)),intent(inout) :: dt1
           type(dtp(4,*)),intent(in) :: dt2

           print *,"in assign1"

           dt1%c1=dt2%c1
           dt1%i1=dt2%i1
           dt1%g1=dt2%g1
           dt1%r1=dt2%r1
       end subroutine

       subroutine assign2(dt1,dt2)
           class(dtp(4,3)),intent(inout) :: dt1(:)
           type(dtp(4,3)),intent(in)    :: dt2

           print *,"in assign2"

           do i=lbound(dt1,1),ubound(dt1,1)

              dt1(i)%c1=dt2%c1
              dt1(i)%i1=dt2%i1
              dt1(i)%g1=dt2%g1
              dt1(i)%r1=dt2%r1

           end do
       end subroutine

       subroutine assign3(dt1,dt2)
           class(dtp(4,*)),intent(inout) :: dt1
           type(dtp(4,3)),intent(in)    :: dt2(:)

           print *,"in assign3"

           dt1=dt2(lbound(dt2,1)) ! call assign1

       end subroutine

       subroutine assign4(dt1,dt2)
           type(dtp(4,3)),intent(inout) :: dt1(:)
           type(dtp(4,*)),intent(in)    :: dt2(:)

           print *,"in assign4"

           dt1=dt2(ubound(dt2,1)) ! call assign2

       end subroutine

       subroutine assign5(dt1,dt2)
           type(dtp(8,*)),intent(inout) :: dt1(:)
           type(dtp(4,*)),intent(in)    :: dt2(:)

           print *,"in aissgn5"

           do i=lbound(dt1,1),ubound(dt1,1)
              dt1(i)%c1="?"//dt2(i)%c1
              dt1(i)%i1=dt2(i)%i1+100
              dt1(i)%g1=dt2(i)%g1 .or. .true.
              dt1(i)%r1=-5.
           end do
       end subroutine

end module

program defAssignIntrinsicComp01a
     use m
     implicit none

     integer :: i
     type(dtp) :: dtp1

     logical,external :: precision_r4,precision_r8

     type(dtp) :: dtp2=dtp(c1="XLF",i1=[10,11], &
                           g1=[.true.,.false.,.true.],&
                           r1=[1.2,-3.5,6.7E-2])

     type(dtp(4,:)),allocatable :: dtp3,dtp4(:),dtp5(:)
     type(dtp(8,3)) :: dtp6(3)

     allocate(dtp(4,3) :: dtp3,dtp4(2:5),dtp5(3))

     dtp1=dtp2 ! call assign1

     if(any(dtp1%c1 /= "XLF"))                                       error stop 10
     if(any(dtp1%i1 /= [10,11]))                                     error stop 11
     if(any(dtp1%g1 .neqv. [.true.,.false.,.true.]))                 error stop 12
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-2),1.2))        error stop 13
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-1),-3.5))       error stop 14
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)),6.7E-2))       error stop 15

     dtp3=dtp1 ! call assign1

     if(any(dtp3%c1 /= "XLF"))                                       error stop 16
     if(any(dtp3%i1 /= [10,11]))                                     error stop 17
     if(any(dtp3%g1 .neqv. [.true.,.false.,.true.]))                 error stop 18
     if(.not. precision_r4(dtp3%r1(ubound(dtp3%r1,1)-2),1.2))        error stop 19
     if(.not. precision_r4(dtp3%r1(ubound(dtp3%r1,1)-1),-3.5))       error stop 20
     if(.not. precision_r4(dtp3%r1(ubound(dtp3%r1,1)),6.7E-2))       error stop 21

     dtp4=dtp3 ! call assign 2

     do i=lbound(dtp4,1),ubound(dtp4,1)

     if(any(dtp4(i)%c1 /= "XLF"))                                    error stop 22
     if(any(dtp4(i)%i1 /= [10,11]))                                  error stop 23
     if(any(dtp4(i)%g1 .neqv. [.true.,.false.,.true.]))              error stop 24
     if(.not. precision_r4(dtp4(i)%r1(ubound(dtp4(i)%r1,1)-2),1.2))  error stop 25
     if(.not. precision_r4(dtp4(i)%r1(ubound(dtp4(i)%r1,1)-1),-3.5)) error stop 26
     if(.not. precision_r4(dtp4(i)%r1(ubound(dtp4(i)%r1,1)),6.7E-2)) error stop 27

     end do

     dtp5=dtp(4,3)()  ! call assign2

     do i=lbound(dtp5,1),ubound(dtp5,1)

     if(any(dtp5(i)%c1 /= "***"))                                    error stop 28
     if(any(dtp5(i)%i1 /= -99))                                      error stop 29
     if(any(dtp5(i)%g1 .neqv. .false.))                              error stop 30
     if(.not. precision_r4(dtp5(i)%r1(ubound(dtp5(i)%r1,1)-2),-9.9)) error stop 31
     if(.not. precision_r4(dtp5(i)%r1(ubound(dtp5(i)%r1,1)-1),-9.9)) error stop 32
     if(.not. precision_r4(dtp5(i)%r1(ubound(dtp5(i)%r1,1)),-9.9))   error stop 33

     end do

     dtp1=[dtp3,dtp1]  ! call assign3

     if(any(dtp1%c1 /= "XLF"))                                       error stop 34
     if(any(dtp1%i1 /= [10,11]))                                     error stop 35
     if(any(dtp1%g1 .neqv. [.true.,.false.,.true.]))                 error stop 36
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-2),1.2))        error stop 37
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)-1),-3.5))       error stop 38
     if(.not. precision_r4(dtp1%r1(ubound(dtp1%r1,1)),6.7E-2))       error stop 39

     dtp4=dtp5  ! call assign4

     do i=lbound(dtp4,1),ubound(dtp4,1)

     if(any(dtp4(i)%c1 /= "***"))                                    error stop 40
     if(any(dtp4(i)%i1 /= -99))                                      error stop 41
     if(any(dtp4(i)%g1 .neqv. .false.))                              error stop 42
     if(.not. precision_r4(dtp4(i)%r1(ubound(dtp4(i)%r1,1)-2),-9.9)) error stop 43
     if(.not. precision_r4(dtp4(i)%r1(ubound(dtp4(i)%r1,1)-1),-9.9)) error stop 44
     if(.not. precision_r4(dtp4(i)%r1(ubound(dtp4(i)%r1,1)),-9.9))   error stop 45

     end do

     dtp6=[ (dtp(4,3)(),i=1,3) ] ! call assign5

     do i=lbound(dtp6,1),ubound(dtp6,1)

     if(any(dtp6(i)%c1 /= "?**"))                                     error stop 46
     if(any(dtp6(i)%i1 /= 1))                                         error stop 47
     if(any(dtp6(i)%g1 .neqv. .true.))                                error stop 48
     if(.not. precision_r8(dtp6(i)%r1(lbound(dtp6(i)%r1,1)),-5._8))   error stop 49
     if(.not. precision_r8(dtp6(i)%r1(lbound(dtp6(i)%r1,1)+1),-5._8)) error stop 50
     if(.not. precision_r8(dtp6(i)%r1(lbound(dtp6(i)%r1,1)+2),-5._8)) error stop 51

     end do

end program
