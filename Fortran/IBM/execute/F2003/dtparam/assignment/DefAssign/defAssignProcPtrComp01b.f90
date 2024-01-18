!*  ===================================================================
!*
!*  DATE                       : Feb. 16 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test defined assignment with generic binding
!* 2. Derived type has procedure pointer components
!* 3. Procedure pointer associates with function
!* 4. Rename derived type in Use statement
!* 5. Defined assignment procedure is elemental procedure
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
   type A(k1,l1)
      integer,kind :: k1
      integer,len  :: l1

      integer(k1) :: i1(l1)=-99
      procedure(ifun),nopass,pointer :: iptr=>null()
      contains
        procedure :: assignA
        generic   :: assignment(=)=>assignA
   end type

   type B(k2,l2)
      integer,kind  :: k2
      integer,len   :: l2

      character(l2) :: c1(l2)="***"
      type(A(2*k2,l2+1)) :: acomp=A(4,3)()
      procedure(afun),nopass,pointer :: aptr=>null()
      contains
         procedure :: assignB
         generic :: assignment(=)=>assignB
   end type

  contains
      integer function ifun(int)
        integer,intent(in) :: int(:)
        allocatable :: ifun(:)

        ifun=-int
      end function

      function afun(ta)
        type(A(4,*)),intent(in) :: ta
        type(A(4,ta%l1))  :: afun

        afun%i1=2*ta%i1
        afun%iptr=>ta%iptr
      end function

      elemental subroutine assignA(this,dt)
         class(A(4,*)),intent(inout) :: this
         class(A(4,*)),intent(in)    :: dt

         this%i1=dt%i1
         this%iptr=>dt%iptr
      end subroutine

      elemental subroutine assignB(this,dt)
         class(B(2,*)),intent(inout) :: this
         class(B(2,*)),intent(in)    :: dt

         this%c1=dt%c1
         this%acomp=dt%acomp ! invoke assignA
         this%aptr=>dt%aptr

      end subroutine

end module

module m2
use m1,XA=>A,XB=>B
end module

program defAssignProcPtrComp01b
     use m2,YA=>XA,YB=>XB
     implicit type(YA(4,3)) (O)
     implicit type(YA(4,3)) (P)
     implicit type(YB(2,2)) (R)
     implicit type(YB(2,2)) (S)

     allocatable :: OA1,RB1
     dimension   :: OA1(:),RB1(:)

     dimension   :: PA2(2),SB2(2)

     print *,"*****TEST  1*****"
     ! invoke assignA3
     PA2= YA(4,3)()
     if(PA2%k1 /= 4)                                      stop 10
     if(PA2%l1 /= 3)                                      stop 11
     if(any(PA2(1)%i1 /= -99))                            stop 12
     if(any(PA2(2)%i1 /= -99))                            stop 13
     if(associated(PA2(1)%iptr))                          stop 14
     if(associated(PA2(2)%iptr))                          stop 15

     print *,"*****TEST  2*****"
     ! invoke assignA
     PA2(1)= YA(4,3)([1,2,3],ifun)

     if(any(PA2(1)%i1 /= [1,2,3]))                        stop 16
     if(.not. associated(PA2(1)%iptr,ifun))               stop 17
     if(any(PA2(1)%iptr([4,5,6]) /= [-4,-5,-6]))          stop 18

     print *,"*****TEST  3*****"
     ! invoke assignA
     PA2(2)= YA(4,3)([-1,-2,-3],ifun)

     if(any(PA2(2)%i1 /= [-1,-2,-3]))                     stop 19
     if(.not. associated(PA2(2)%iptr,ifun))               stop 20
     if(any(PA2(2)%iptr(PA2(2)%i1) /= [1,2,3]))           stop 21

     print *,"*****TEST  4*****"
     ! invoke assignA
     PA2 = PA2(2:1:-1)

     if(any(PA2(1)%i1 /= [-1,-2,-3]))                     stop 22
     if(any(PA2(2)%i1 /= [1,2,3]))                        stop 23
     if(.not. associated(PA2(1)%iptr,ifun))               stop 24
     if(.not. associated(PA2(2)%iptr,ifun))               stop 25

     allocate(YA(4,PA2%l1) :: OA1(size(PA2)) )

     print *,"*****TEST  5*****"
     ! invoke assignA
     OA1=PA2

     if(OA1%k1 /= 4)                                      stop 26
     if(OA1%l1 /= 3)                                      stop 27
     if(any(OA1(1)%i1 /= [-1,-2,-3]))                     stop 28
     if(any(OA1(2)%i1 /= [1,2,3]))                        stop 29
     if(.not. associated(OA1(1)%iptr,ifun))               stop 30
     if(.not. associated(OA1(2)%iptr,ifun))               stop 31
     if(any(OA1(1)%iptr([100]) /= -100))                  stop 32
     if(any(OA1(2)%iptr([10,-10]) /= [-10,10]))           stop 33

     print *,"*****TEST  6*****"
     ! invoke assignB
     SB2(1)=YB(2,2)()

     if(any(SB2(1)%c1 /= "**"))                           stop 34
     if(any(SB2(1)%acomp%i1 /= -99))                      stop 35
     if(associated(SB2(1)%acomp%iptr))                    stop 36
     if(associated(SB2(1)%aptr))                          stop 37

     print *,"*****TEST  7*****"
     ! invoke assignB
     SB2=[YB(2,2)(),YB(2,2)()]

     if(any(SB2(2)%c1 /= "**"))                           stop 38
     if(any(SB2(2)%acomp%i1 /= -99))                      stop 39
     if(associated(SB2(2)%acomp%iptr))                    stop 40
     if(associated(SB2(2)%aptr))                          stop 41

     print *,"*****TEST  8*****"
     ! invoke assignB
     SB2=YB(2,2)()

     if(any(SB2(1)%c1 /= "**"))                           stop 42
     if(any(SB2(1)%acomp%i1 /= -99))                      stop 43
     if(associated(SB2(1)%acomp%iptr))                    stop 44
     if(associated(SB2(1)%aptr))                          stop 45

     if(any(SB2(2)%c1 /= "**"))                           stop 46
     if(any(SB2(2)%acomp%i1 /= -99))                      stop 47
     if(associated(SB2(2)%acomp%iptr))                    stop 48
     if(associated(SB2(2)%aptr))                          stop 49

     print *,"*****TEST  9*****"
     ! invoke assignB
     SB2(1)=YB(2,2)("ab",YA(4,3)([11,12,13],ifun),afun)

     if(any(SB2(1)%c1 /= "ab"))                           stop 50
     if(any(SB2(1)%acomp%i1 /= [11,12,13]))               stop 51
     if(.not. associated(SB2(1)%acomp%iptr))              stop 52
     if(.not. associated(SB2(1)%aptr))                    stop 53
     if(any(SB2(1)%acomp%iptr(SB2(1)%acomp%i1) /= &
                   [-11,-12,-13] ))                       stop 54

     associate(x=>SB2(1)%aptr(YA(4,3)([22,23,24],null()) ) )
        if(any(x%i1 /= [44,46,48]))                       stop 55
        if(associated(x%iptr))                            stop 56
     end associate

     print *,"*****TEST 10*****"
     ! invoke assignB
     SB2(2)=YB(2,2)("AB",OA1(2),afun)

     if(any(SB2(2)%c1 /= "AB"))                           stop 57
     if(any(SB2(2)%acomp%i1 /= [1,2,3]))                  stop 58
     if(.not. associated(SB2(2)%acomp%iptr))              stop 59
     if(.not. associated(SB2(2)%aptr))                    stop 60
     if(any(SB2(1)%acomp%iptr(SB2(2)%acomp%i1) /= &
                   [-1,-2,-3] ))                          stop 61

     associate(x=>SB2(2)%aptr(YA(4,3)([-1,-2,-3],ifun) ) )
        if(any(x%i1 /= [-2,-4,-6]))                       stop 62
        if(.not. associated(x%iptr))                      stop 63
        if(any(x%iptr([-5]) /= 5))                        stop 64
     end associate

     print *,"*****TEST 11*****"
     ! invoke assignB
     SB2=[YB(2,2)("cd",PA2(1),afun),YB(2,2)("CD",OA1(1),afun)]

     if(any(SB2(1)%c1 /= "cd"))                           stop 65
     if(any(SB2(1)%acomp%i1 /= [-1,-2,-3]))               stop 66
     if(.not. associated(SB2(1)%acomp%iptr))              stop 67
     if(.not. associated(SB2(1)%aptr))                    stop 68
     if(any(SB2(1)%acomp%iptr([3,4]) /= &
                   [-3,-4] ))                             stop 69

     associate(x=>SB2(1)%aptr(YA(4,3)([-1,-2,-3],ifun) ) )
        if(any(x%i1 /= [-2,-4,-6]))                       stop 70
        if(.not. associated(x%iptr))                      stop 71
        if(any(x%iptr([-5]) /= 5))                        stop 72
     end associate

     if(any(SB2(2)%c1 /= "CD"))                           stop 73
     if(any(SB2(2)%acomp%i1 /= [-1,-2,-3]))               stop 74
     if(.not. associated(SB2(2)%acomp%iptr))              stop 75
     if(.not. associated(SB2(2)%aptr))                    stop 76
     if(any(SB2(1)%acomp%iptr(SB2(2)%acomp%i1) /= &
                   [1,2,3] ))                             stop 77

     associate(x=>SB2(1)%aptr(OA1(2)) )
        if(any(x%i1 /= [2,4,6]))                          stop 78
        if(.not. associated(x%iptr,ifun))                 stop 79
     end associate

     allocate(YB(2,2) :: RB1(1:1))

     print *,"*****TEST 12*****"
     ! invoke assignB
     RB1=SB2(1)

     if(any(RB1(1)%c1 /= "cd"))                           stop 80
     if(any(RB1(1)%acomp%i1 /= [-1,-2,-3]))               stop 81
     if(.not. associated(RB1(1)%acomp%iptr))              stop 82
     if(.not. associated(RB1(1)%aptr))                    stop 83
     if(any(RB1(1)%acomp%iptr([3,4]) /= &
                   [-3,-4] ))                             stop 84

     associate(x=>RB1(1)%aptr(YA(4,3)([-1,-2,-3],ifun) ) )
        if(any(x%i1 /= [-2,-4,-6]))                       stop 85
        if(.not. associated(x%iptr))                      stop 86
        if(any(x%iptr([-5]) /= 5))                        stop 87
     end associate

     print *,"*****TEST 13*****"
     ! invoke assignB
     RB1=SB2(2:2)

     if(any(RB1(1)%c1 /= "CD"))                           stop 88
     if(any(RB1(1)%acomp%i1 /= [-1,-2,-3]))               stop 89
     if(.not. associated(RB1(1)%acomp%iptr))              stop 90
     if(.not. associated(RB1(1)%aptr))                    stop 91
     if(any(RB1(1)%acomp%iptr(RB1(1)%acomp%i1) /= &
                   [1,2,3] ))                             stop 92

     associate(x=>RB1(1)%aptr(OA1(2)) )
        if(any(x%i1 /= [2,4,6]))                          stop 93
        if(.not. associated(x%iptr,ifun))                 stop 94
     end associate

     print *,"*****TEST 14*****"
     ! invoke assignB
     RB1(1)=SB2(1)

     if(any(RB1(1)%c1 /= "cd"))                           stop 95
     if(any(RB1(1)%acomp%i1 /= [-1,-2,-3]))               stop 96
     if(.not. associated(RB1(1)%acomp%iptr))              stop 97
     if(.not. associated(RB1(1)%aptr))                    stop 98
     if(any(RB1(1)%acomp%iptr([3,4]) /= &
                   [-3,-4] ))                             stop 99

     associate(x=>RB1(1)%aptr(YA(4,3)([-1,-2,-3],ifun) ) )
        if(any(x%i1 /= [-2,-4,-6]))                       stop 100
        if(.not. associated(x%iptr))                      stop 101
        if(any(x%iptr([-5]) /= 5))                        stop 102
     end associate

end program
