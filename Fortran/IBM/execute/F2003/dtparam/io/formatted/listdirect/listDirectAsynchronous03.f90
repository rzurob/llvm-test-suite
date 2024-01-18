!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 26 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test read statement with asynchronous IO and stream access
!* 2. Derived type is polymorphic type and has multiple derived type components
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
  type A(len)
     integer,len  :: len
     integer      :: i1(len:len+1)=-99
     character(3) :: c1(len-1:len)="***"
     logical      :: g1(len:len+1)=.false.
     real         :: r1=-99.9
     complex      :: x1=(-99.9,99.9)
  end type

  type base(l1)
    integer,len :: l1

    type(A(l1)) :: a1comp(l1+1)
    type(A(l1)) :: a2comp(l1)

    contains

      procedure :: readDT=>readBase
      generic  :: read=>readDT
  end type

  type,extends(base) :: child(l2)
    integer,len  :: l2
    type(A(l2))      :: a3comp(l2)
    type(A(l2))      :: a4comp(l2-1)
    contains

       procedure :: readDT=>readChild
       generic  :: read=>readDT
  end type

  contains

    subroutine readBase(this,unit,idvar,mypos)
       class(base(*)),intent(inout) :: this
       integer,intent(in) :: unit
       integer :: idvar ,mypos

       select type(this)
         type is(base(*))
            read(unit,*,asynchronous='yes', &
               id=idvar,pos=mypos,decimal='comma') this
         class default
            stop 16
       end select
    end subroutine

    subroutine readChild(this,unit,idvar,mypos)
        class(child(*,*)),intent(inout) :: this
        integer,intent(in) :: unit
        integer :: idvar,mypos

      select type(this)
        type is(child(*,*))
           read(unit,*,asynchronous='yes', &
               id=idvar,pos=mypos) this%a3comp,this%a4comp
        class default
           stop 17
      end select

    end subroutine

end module

module m2
  use m1

    contains

    subroutine readData(dt,unit)
       class(base(*)),intent(inout) :: dt
       integer,intent(in)  :: unit

       integer :: mypos,idvar
       logical :: pending

       select type(dt)
          type is(child(*,*))
             inquire(unit,pos=mypos)

             if(mypos /= 1)                    stop 12

             call dt%read(unit,idvar,mypos)

             wait(unit,id=idvar)

             inquire(unit,id=idvar,pending=pending,pos=mypos)

             if(pending .neqv. .false.)        stop 13
             if(mypos /= 135)                  stop 14

             call dt%base%read(unit,idvar,mypos)

             wait(unit,id=idvar)

             inquire(unit,id=idvar,pending=pending,pos=mypos)

             if(pending .neqv. .false.)        stop 15

           class default

             stop 11
       end select

    end subroutine

end module

program listDirectAsynchronous03
  use m2
  implicit none

  integer :: ios,unit=10
  character(256) :: msg
  logical,external :: precision_r4,precision_x8

  class(base(:)),allocatable :: pobj1(:,:)

  allocate(child(1,2) :: pobj1(-1:-1,1) )

  open(unit=unit,file='listDirectAsynchronous03.dat',status='old',&
        form='formatted',access='stream', asynchronous='yes',&
        iostat=ios,iomsg=msg)

  if(ios <> 0) then

     print *,"fail to open the file"
     print *,"iostat=",ios
     print *,"iomsg=",msg
     stop 10

  end if

  ! following is the data we want to read

  !-1 -2 , 123 4.5 .tea fall 0.1 (-3.1, 0.5)
  !1*2,, 2*"RED" 2*T 2*
  !-8
  !-10
  !"GOOD" ,
  !WHO
  !.TTTTTTTT      ,
  !FLOOR
  !5.5556   ,
  ! ( 1.3
  !, -4.1 )
  !-50 ; +6 XLF 'xlf' T F -12,3 ; ( -0,35E-3 ; +2,E0  )
  !3 ; ; "IBM" ibm .false. .true. ; -5, ; 1*;
  !; +10 "
  !ALC" 'al
  !c' ttt to ; -3,E1 ; ( -3,2 ; -1, )

  call readData(pobj1(-1,1),unit)

  ! verify data

  select type(x=>pobj1(-1,1) )
     type is(child(*,*))
         if(any(x%a1comp(1)%i1 /= [-50,6]))                      stop 19
         if(any(x%a1comp(1)%c1 /= ["XLF","xlf"] ))               stop 20
         if(any(x%a1comp(1)%g1 .neqv. [.true.,.false.]))         stop 21

         if(.not. precision_r4(x%a1comp(1)%r1,-12.3_4))          stop 22
         if(.not. precision_r4(x%a1comp(1)%x1,(-0.35E-3,2.E0) )) stop 23

         if(any(x%a1comp(2)%i1 /= [3,-99]))                      stop 24
         if(any(x%a1comp(2)%c1 /= ["IBM","ibm"] ))               stop 25
         if(any(x%a1comp(2)%g1 .neqv. [.false.,.true.]))         stop 26

         if(.not. precision_r4(x%a1comp(2)%r1,-5.))              stop 27
         if(.not. precision_r4(x%a1comp(2)%x1,(-99.9,99.9) ))    stop 28

         associate(y=>x%a2comp(1))

         if(any(y%i1 /= [-99,10]))                               stop 29
         if(any(y%c1 /= ["ALC","alc"] ))                         stop 30
         if(any(y%g1 .neqv. [.true.,.true.]))                    stop 31

         if(.not. precision_r4(y%r1,-30.))                       stop 32
         if(.not. precision_r4(y%x1,(-3.2,-1.) ))                stop 33

         end associate

         if(any(x%a3comp(1)%i1 /= [-1,-2]))                      stop 34
         if(any(x%a3comp(1)%c1 /= ["123","4.5"] ))               stop 35
         if(any(x%a3comp(1)%g1 .neqv. [.true.,.false.]))         stop 36

         if(.not. precision_r4(x%a3comp(1)%r1,0.1_4))            stop 37
         if(.not. precision_r4(x%a3comp(1)%x1,(-3.1,0.5) ))      stop 38

         if(any(x%a3comp(2)%i1 /= [2,-99]))                      stop 39
         if(any(x%a3comp(2)%c1 /= ["RED","RED"] ))               stop 40
         if(any(x%a3comp(2)%g1 .neqv. [.true.,.true.]))          stop 41

         if(.not. precision_r4(x%a3comp(2)%r1,-99.9))            stop 42
         if(.not. precision_r4(x%a3comp(2)%x1,(-99.9,99.9) ))    stop 43

         if(any(x%a4comp(1)%i1 /= [-8,-10]))                     stop 44
         if(any(x%a4comp(1)%c1 /= ["GOO","WHO"] ))               stop 45
         if(any(x%a4comp(1)%g1 .neqv. [.true.,.false.]))         stop 46

         if(.not. precision_r4(x%a4comp(1)%r1,5.5556))           stop 47
         if(.not. precision_r4(x%a4comp(1)%x1,(1.3,-4.1) ))      stop 48

     class default

        stop 18
  end select

  close(10)

end program
