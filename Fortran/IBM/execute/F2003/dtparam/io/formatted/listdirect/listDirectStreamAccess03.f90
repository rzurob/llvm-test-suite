!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : listDirectStreamAccess03.f
!*
!*  DATE                       : Jan. 21 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : LIST-DIRECTED INTRINSIC IO
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test Read statement with list directed IO  and stream access
!* 2. Execute read in type bound procedure
!* 3. Derived type is polymorphic type
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
   type A(k1,l1)
     integer,kind :: k1 !k1=4
     integer,len  :: l1 !l1=3

     character(l1):: c1(k1)="***"
     real(k1)    :: r1(l1)=9.9
     contains
       procedure :: readDT=>readA
       generic   :: read=>readDT

   end type
   type base(k2,l2)
     integer,kind :: k2 ! k2=8
     integer,len  :: l2 ! l2=4

     complex(k2)  :: x1(l2)=(-9.9_8,-9.9_8)
     type(A(k2/2,l2-1)) :: a1comp

     contains
       procedure :: readDT=>readBase
       generic   :: read=>readDT
   end type

   contains

       subroutine readA(this,unit)
          class(A(4,*)),intent(inout) :: this
          integer,intent(in) :: unit

          print *,"in readA"
          select type(this)
             type is(A(4,*))
               read(unit,*,decimal='comma') this%c1,this%r1
             class default
               stop 11
          end select

       end subroutine

       subroutine readBase(this,unit,bound)
          class(base(8,*)),intent(inout) :: this
          integer,intent(in) :: unit,bound

          print *,"in readBase"

          read(unit,*,decimal='point') this%x1
          call this%a1comp%read(unit)

       end subroutine

end module

module m2
  use m1

  type,extends(base) :: child(k3,l3)
     integer,kind :: k3 ! k3=4
     integer,len  :: l3 ! l3=3

     integer(k3) :: i1(l3-1)=-99
     type(A(k3,l3)) :: a2comp

     contains
        procedure :: readDT=>readChild
        generic  :: read=>readDT
  end type

  contains

    subroutine readChild(this,unit,bound)
       class(child(8,*,4,*)),intent(inout) :: this
       integer,intent(in)  :: unit,bound

       print *,"in readChild"

       if(bound .eq. 2) then

          call this%base%read(unit,bound)
          select type(this)
             type is(child(8,*,4,*))
                read(unit,*)   this%i1
                call this%a2comp%read(unit)
             class default
                stop 12
          end select

        else

           select type(this)
              type is(child(8,*,4,*))
                  read(unit,*) this
              class default
                  stop 13
           end select

        end if

    end subroutine

end module

program listDirectStreamAccess03
    use m2

   interface
      subroutine sub(arg)
        import
        class(base(8,*)),intent(inout) :: arg(2:)
      end subroutine
   end interface

   logical,external :: precision_x6,precision_r4

   class(base(8,:)),pointer :: ptr(:)
   type(child(8,:,4,:)),target,allocatable :: tar(:)

   allocate(child(8,tar%k2/2,4,tar%k3-1) :: tar(-1:0) )

   ptr(2:)=>tar(0:-1:-1)

   ! following is the record need to read

   !1*(  -3.5E-2 ,  3.5 ) , (-2.1D-10,
   !-0.01D-5 ) (-98121.3
   !, -2E05) (0.4,-4.5)
   !; "    A    " BCDE  'FGH'; -7,2 5 -3,;
   !+12 ,,
   !XLF '''A''
   !' "IBM" "
   !""B""" 2*-13,3E-2;;
   !1*, 2*(1.2,-1.2) 1*(-0.5,0.5) 2*"ABC" , 1*HOT
   !1*'CAP' , -4.5, -0.007 -34.E4
   !-4 +8 abc "def" 'ghi' j_k 1* -2.8 09

   call sub(tar)

   !verify value of ptr

   select type(x=>ptr)
      type is(child(8,*,4,*))

        if(.not. precision_x6(x(2)%x1(1),(-9.9_8,-9.9_8)))     stop 15
        if(.not. precision_x6(x(2)%x1(2),(1.2_8,-1.2_8)))      stop 16
        if(.not. precision_x6(x(2)%x1(3),(1.2_8,-1.2_8)))      stop 17
        if(.not. precision_x6(x(2)%x1(4),(-0.5_8,0.5_8)))      stop 18

        if(any(x(2)%a1comp%c1 /= (/"ABC","ABC","HOT","CAP" /) ))   stop 19
        if(.not. precision_r4(x(2)%a1comp%r1(1),-4.5 ))        stop 20
        if(.not. precision_r4(x(2)%a1comp%r1(2),-0.007 ))      stop 21
        if(.not. precision_r4(x(2)%a1comp%r1(3),-34.E4 ))      stop 22

        if(any(x(2)%i1 /= [-4,8] ))                            stop 23
        if(any(x(2)%a2comp%c1 /= ["abc","def","ghi","j_k"] ))  stop 24

        if(.not. precision_x6(x(3)%x1(1),(-3.5D-2,3.5_8)))     stop 25
        if(.not. precision_x6(x(3)%x1(2),(-2.1D-10,-0.01D-5))) stop 26
        if(.not. precision_x6(x(3)%x1(3),(-98121.3_8,-2.D05))) stop 27
        if(.not. precision_x6(x(3)%x1(4),(0.4_8,-4.5_8)))      stop 28

        if(any(x(3)%a1comp%c1 /= &
                 (/"***","   ","BCD","FGH" /) ))               stop 29
        if(.not. precision_r4(x(3)%a1comp%r1(1),-7.2 ))        stop 30
        if(.not. precision_r4(x(3)%a1comp%r1(2),5._4 ))        stop 31
        if(.not. precision_r4(x(3)%a1comp%r1(3),-3. ))         stop 32

        if(any(x(3)%i1 /= [12,-99] ))                          stop 33
        if(any(x(3)%a2comp%c1 /= ["XLF","\'A\'","IBM","\"B\""] ))  stop 34

      class default
        stop 14
   end select
end program

subroutine sub(arg)
   use m2
   class(base(8,*)),intent(inout) :: arg(2:)

   integer :: ios
   character(256) :: msg

   open(10,file='listDirectStreamAccess03.dat',form='formatted',&
       access='stream',status='old',iostat=ios,iomsg=msg)

   if(ios <> 0) then

      print *,"fail to open the file"
      print *,"iostat=",ios
      print *,"iomsg=",msg
      stop 10

   end if

   call arg(2)%read(10,2)

   call arg(3)%read(10,3)

   close(10,status='keep')

end subroutine
