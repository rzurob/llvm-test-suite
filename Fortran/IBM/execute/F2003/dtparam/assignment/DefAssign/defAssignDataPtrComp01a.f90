!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : defAssignDataPtrComp01a.f   
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Nancy Wang 
!*  DATE                       : Feb. 9 2009 
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT 
!*
!*  SECONDARY FUNCTIONS TESTED :  
!*
!*  REFERENCE                  : 
!*
!*  DRIVER STANZA              : xlf2003
!*
!*
!*  DESCRIPTION
!* 1. Test defined assignment with interface block
!* 2. Components are pointers to intrinsic type
!* 3. Use Entry statement 
!234567490123456749012345674901234567490123456749012345674901234567490
module m
   type dtp(k1,l1)
      integer,kind :: k1
      integer,len  :: l1

      character(l1),pointer :: cptr1(:)=>null()
      character(:),pointer  :: cptr2(:)=>null()
      integer(k1),pointer   :: iptr(:)=>null()
      logical(k1),pointer   :: gptr(:)=>null() 

   end type

   interface assignment(=)
      module procedure assignDT1,assignDT2,assignDT3, &
                       assignChar,assignInt,assignLog
   end interface

   contains
       
        subroutine assignDT1(this,dt)
            type(dtp(4,*)),intent(inout) :: this
            type(dtp(4,*)),intent(in)    :: dt

            print *,"in assignDT1"            
            this%cptr1=>dt%cptr1
            this%cptr2=>dt%cptr2
            this%iptr=>dt%iptr
            this%gptr=>dt%gptr
 
        end subroutine

        subroutine assignDT2(this,dt)
            type(dtp(4,*)),intent(inout) :: this(:)
            type(dtp(4,*)),intent(in)    :: dt(:)

            print *,"in assignDT2"
            do i=lbound(this,1),ubound(this,1)
                this(i)%cptr1=>dt(i)%cptr1
                this(i)%cptr2=>dt(i)%cptr2
                this(i)%iptr=>dt(i)%iptr
                this(i)%gptr=>dt(i)%gptr
            end do

        end subroutine

        subroutine assignDT3(this,dt)
            type(dtp(4,*)),intent(inout) :: this(:)
            type(dtp(4,*)),intent(in)    :: dt

            print *,"in assignDT3"
            do i=lbound(this,1),ubound(this,1)
                this(i)%cptr1=>dt%cptr1
                this(i)%cptr2=>dt%cptr2
                this(i)%iptr=>dt%iptr
                this(i)%gptr=>dt%gptr
            end do
        end subroutine

        subroutine assignChar(this,char)
            type(dtp(4,*)),intent(inout) :: this
            character(*),intent(in)      :: char(:)
            integer,intent(in)           :: int(:)
            logical,intent(in)           :: log(:)

            print *,"in assignChar" 
            allocate(this%cptr1(size(char)),source=char)
            allocate(character(2*char%len) :: this%cptr2(size(char)))

            this%cptr2=char // char

            goto 100

            entry assignInt(this,int)

            print *,"in assignInt"

            allocate(this%iptr(size(int)))
 
            this%iptr=int

            goto 100

            entry assignLog(this,log)

            print *,"in assignLog"
       
            allocate(this%gptr(size(log)))

            this%gptr=log

100         return

        end subroutine
 
end module

program defAssignDataPtrComp01a
     use m
     implicit none

     type(dtp(4,3)),target :: tar1(2)

     type(dtp(4,:)),pointer :: ptr1(:)=>null(),ptr2=>null()

     type(dtp(4,3)),pointer :: ptr3(:)

     character(3),target :: ctar1(5)=["hat","cat","get","fat","net"]
     character(4),target :: ctar2(4)=["abcd","ABCD","efgh","EFGH"]

     integer,target :: itar(6) = [1,2,3,4,5,6]
     logical,target :: gtar(4) = [.true.,.false.,.true.,.false.]

     ! call assignDT2 
     tar1=[dtp(4,3)(ctar1(1:2),ctar2(1:2),itar(1:4),gtar(1:3)), &
           dtp(4,3)(ctar1(3:5),ctar2(3:4),itar(5:6),gtar(4:4)) ]  

     ! verify tar1
     if(tar1(1)%cptr1%len /= 3)                          stop 10
     if(lbound(tar1(1)%cptr1,1) /= 1 .or. &
        ubound(tar1(1)%cptr1,1) /= 2)                    stop 11

     if(any(tar1(1)%cptr1 /= ["hat","cat"]))             stop 12
     if(any(tar1(1)%cptr2 /= ["abcd","ABCD"]))           stop 13

     if(any(tar1(1)%iptr /= [1,2,3,4]))                  stop 14
     if(any(tar1(1)%gptr .neqv. [.true.,.false.,.true.]))    stop 15

     if(tar1(2)%cptr1%len /= 3)                          stop 16
     if(lbound(tar1(2)%cptr1,1) /= 1 .or. &
        ubound(tar1(2)%cptr1,1) /= 3)                    stop 17

     if(any(tar1(2)%cptr1 /= ["get","fat","net"]))       stop 18
     if(any(tar1(2)%cptr2 /= ["efgh","EFGH"]))           stop 19

     if(any(tar1(2)%iptr /= [5,6]))                      stop 20
     if(any(tar1(2)%gptr .neqv. [.false.]))              stop 21

     allocate(dtp(4,3) :: ptr1(3:4)) 

     ! call assignDT2
     ptr1=tar1(2:1:-1)

     ! verify ptr1
     if(ptr1(4)%cptr1%len /= 3)                          stop 22
     if(lbound(ptr1(4)%cptr1,1) /= 1 .or. &
        ubound(ptr1(4)%cptr1,1) /= 2)                    stop 23
     if(any(ptr1(4)%cptr1 /= ["hat","cat"]))             stop 24
     if(any(ptr1(4)%cptr2 /= ["abcd","ABCD"]))           stop 25

     if(any(ptr1(4)%iptr /= [1,2,3,4]))                  stop 26
     if(any(ptr1(4)%gptr .neqv. [.true.,.false.,.true.])) stop 27

     if(ptr1(3)%cptr1%len /= 3)                          stop 28
     if(lbound(ptr1(3)%cptr1,1) /= 1 .or. &
        ubound(ptr1(3)%cptr1,1) /= 3)                    stop 29

     if(any(ptr1(3)%cptr1 /= ["get","fat","net"]))      stop 30
     if(any(ptr1(3)%cptr2 /= ["efgh","EFGH"]))           stop 31

     if(any(ptr1(3)%iptr /= [5,6]))                      stop 32
     if(any(ptr1(3)%gptr .neqv. [.false.]))              stop 33

     allocate(ptr3(-1:-1))

     ! call assignDT3
     ptr3(-1:-1)=dtp(4,3)(ctar1(2:3),ctar2(3:4),itar(5:6),gtar(3:4))

     ! verify ptr3
     if(ptr3(-1)%cptr1%len /= 3)                         stop 34
     if(any(ptr3(-1)%cptr1 /= ["cat","get"]))            stop 35
     if(any(ptr3(-1)%cptr2 /= ["efgh","EFGH"]))          stop 36
     if(any(ptr3(-1)%iptr /= [5,6]))                     stop 37
     if(any(ptr3(-1)%gptr .neqv. [.true.,.false.]))      stop 38

     deallocate(ptr3)

     allocate(ptr3(1))

     ! call assignDT1
     ptr3(1)=dtp(4,3)(ctar1(5:1:-2),ctar2(4:1:-2),itar(6:1:-2),gtar(4:1:-2))

     !verify ptr3
     if(ptr3(1)%cptr1%len /= 3)                         stop 39
     if(any(ptr3(1)%cptr1 /= ["net","get","hat"]))      stop 40
     if(any(ptr3(1)%cptr2 /= ["EFGH","ABCD"]))          stop 41
     if(any(ptr3(1)%iptr /= [6,4,2]))                   stop 42
     if(any(ptr3(1)%gptr .neqv. [.false.,.false.]))     stop 43

     allocate(dtp(ptr3%k1,ptr3%l1) :: ptr2)
   
     ! call assignChar
     ptr2=ctar1(2:3)      

     ! call assignInt
     ptr2=itar

     ! call assignLog
     ptr2=gtar(1:4:2)       

     !verify ptr2
     if(ptr2%cptr1%len /= 3)                            stop 44
     if(any(ptr2%cptr1 /= ["cat","get"]))               stop 45
     if(any(ptr2%cptr2 /= ["catcat","getget"]))         stop 46
     if(any(ptr2%iptr /=  [1,2,3,4,5,6]))               stop 47
     if(any(ptr2%gptr .neqv. [.true.,.true.]))          stop 48                                
end program
