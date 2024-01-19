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
     if(PA2%k1 /= 4)                                      error stop 10
     if(PA2%l1 /= 3)                                      error stop 11
     if(any(PA2(1)%i1 /= -99))                            error stop 12
     if(any(PA2(2)%i1 /= -99))                            error stop 13
     if(associated(PA2(1)%iptr))                          error stop 14
     if(associated(PA2(2)%iptr))                          error stop 15

     print *,"*****TEST  2*****"
     ! invoke assignA
     PA2(1)= YA(4,3)([1,2,3],ifun)

     if(any(PA2(1)%i1 /= [1,2,3]))                        error stop 16
     if(.not. associated(PA2(1)%iptr,ifun))               error stop 17
     if(any(PA2(1)%iptr([4,5,6]) /= [-4,-5,-6]))          error stop 18

     print *,"*****TEST  3*****"
     ! invoke assignA
     PA2(2)= YA(4,3)([-1,-2,-3],ifun)

     if(any(PA2(2)%i1 /= [-1,-2,-3]))                     error stop 19
     if(.not. associated(PA2(2)%iptr,ifun))               error stop 20
     if(any(PA2(2)%iptr(PA2(2)%i1) /= [1,2,3]))           error stop 21

     print *,"*****TEST  4*****"
     ! invoke assignA
     PA2 = PA2(2:1:-1)

     if(any(PA2(1)%i1 /= [-1,-2,-3]))                     error stop 22
     if(any(PA2(2)%i1 /= [1,2,3]))                        error stop 23
     if(.not. associated(PA2(1)%iptr,ifun))               error stop 24
     if(.not. associated(PA2(2)%iptr,ifun))               error stop 25

     allocate(YA(4,PA2%l1) :: OA1(size(PA2)) )

     print *,"*****TEST  5*****"
     ! invoke assignA
     OA1=PA2

     if(OA1%k1 /= 4)                                      error stop 26
     if(OA1%l1 /= 3)                                      error stop 27
     if(any(OA1(1)%i1 /= [-1,-2,-3]))                     error stop 28
     if(any(OA1(2)%i1 /= [1,2,3]))                        error stop 29
     if(.not. associated(OA1(1)%iptr,ifun))               error stop 30
     if(.not. associated(OA1(2)%iptr,ifun))               error stop 31
     if(any(OA1(1)%iptr([100]) /= -100))                  error stop 32
     if(any(OA1(2)%iptr([10,-10]) /= [-10,10]))           error stop 33

     print *,"*****TEST  6*****"
     ! invoke assignB
     SB2(1)=YB(2,2)()

     if(any(SB2(1)%c1 /= "**"))                           error stop 34
     if(any(SB2(1)%acomp%i1 /= -99))                      error stop 35
     if(associated(SB2(1)%acomp%iptr))                    error stop 36
     if(associated(SB2(1)%aptr))                          error stop 37

     print *,"*****TEST  7*****"
     ! invoke assignB
     SB2=[YB(2,2)(),YB(2,2)()]

     if(any(SB2(2)%c1 /= "**"))                           error stop 38
     if(any(SB2(2)%acomp%i1 /= -99))                      error stop 39
     if(associated(SB2(2)%acomp%iptr))                    error stop 40
     if(associated(SB2(2)%aptr))                          error stop 41

     print *,"*****TEST  8*****"
     ! invoke assignB
     SB2=YB(2,2)()

     if(any(SB2(1)%c1 /= "**"))                           error stop 42
     if(any(SB2(1)%acomp%i1 /= -99))                      error stop 43
     if(associated(SB2(1)%acomp%iptr))                    error stop 44
     if(associated(SB2(1)%aptr))                          error stop 45

     if(any(SB2(2)%c1 /= "**"))                           error stop 46
     if(any(SB2(2)%acomp%i1 /= -99))                      error stop 47
     if(associated(SB2(2)%acomp%iptr))                    error stop 48
     if(associated(SB2(2)%aptr))                          error stop 49

     print *,"*****TEST  9*****"
     ! invoke assignB
     SB2(1)=YB(2,2)("ab",YA(4,3)([11,12,13],ifun),afun)

     if(any(SB2(1)%c1 /= "ab"))                           error stop 50
     if(any(SB2(1)%acomp%i1 /= [11,12,13]))               error stop 51
     if(.not. associated(SB2(1)%acomp%iptr))              error stop 52
     if(.not. associated(SB2(1)%aptr))                    error stop 53
     if(any(SB2(1)%acomp%iptr(SB2(1)%acomp%i1) /= &
                   [-11,-12,-13] ))                       error stop 54

     associate(x=>SB2(1)%aptr(YA(4,3)([22,23,24],null()) ) )
        if(any(x%i1 /= [44,46,48]))                       error stop 55
        if(associated(x%iptr))                            error stop 56
     end associate

     print *,"*****TEST 10*****"
     ! invoke assignB
     SB2(2)=YB(2,2)("AB",OA1(2),afun)

     if(any(SB2(2)%c1 /= "AB"))                           error stop 57
     if(any(SB2(2)%acomp%i1 /= [1,2,3]))                  error stop 58
     if(.not. associated(SB2(2)%acomp%iptr))              error stop 59
     if(.not. associated(SB2(2)%aptr))                    error stop 60
     if(any(SB2(1)%acomp%iptr(SB2(2)%acomp%i1) /= &
                   [-1,-2,-3] ))                          error stop 61

     associate(x=>SB2(2)%aptr(YA(4,3)([-1,-2,-3],ifun) ) )
        if(any(x%i1 /= [-2,-4,-6]))                       error stop 62
        if(.not. associated(x%iptr))                      error stop 63
        if(any(x%iptr([-5]) /= 5))                        error stop 64
     end associate

     print *,"*****TEST 11*****"
     ! invoke assignB
     SB2=[YB(2,2)("cd",PA2(1),afun),YB(2,2)("CD",OA1(1),afun)]

     if(any(SB2(1)%c1 /= "cd"))                           error stop 65
     if(any(SB2(1)%acomp%i1 /= [-1,-2,-3]))               error stop 66
     if(.not. associated(SB2(1)%acomp%iptr))              error stop 67
     if(.not. associated(SB2(1)%aptr))                    error stop 68
     if(any(SB2(1)%acomp%iptr([3,4]) /= &
                   [-3,-4] ))                             error stop 69

     associate(x=>SB2(1)%aptr(YA(4,3)([-1,-2,-3],ifun) ) )
        if(any(x%i1 /= [-2,-4,-6]))                       error stop 70
        if(.not. associated(x%iptr))                      error stop 71
        if(any(x%iptr([-5]) /= 5))                        error stop 72
     end associate

     if(any(SB2(2)%c1 /= "CD"))                           error stop 73
     if(any(SB2(2)%acomp%i1 /= [-1,-2,-3]))               error stop 74
     if(.not. associated(SB2(2)%acomp%iptr))              error stop 75
     if(.not. associated(SB2(2)%aptr))                    error stop 76
     if(any(SB2(1)%acomp%iptr(SB2(2)%acomp%i1) /= &
                   [1,2,3] ))                             error stop 77

     associate(x=>SB2(1)%aptr(OA1(2)) )
        if(any(x%i1 /= [2,4,6]))                          error stop 78
        if(.not. associated(x%iptr,ifun))                 error stop 79
     end associate

     allocate(YB(2,2) :: RB1(1:1))

     print *,"*****TEST 12*****"
     ! invoke assignB
     RB1=SB2(1)

     if(any(RB1(1)%c1 /= "cd"))                           error stop 80
     if(any(RB1(1)%acomp%i1 /= [-1,-2,-3]))               error stop 81
     if(.not. associated(RB1(1)%acomp%iptr))              error stop 82
     if(.not. associated(RB1(1)%aptr))                    error stop 83
     if(any(RB1(1)%acomp%iptr([3,4]) /= &
                   [-3,-4] ))                             error stop 84

     associate(x=>RB1(1)%aptr(YA(4,3)([-1,-2,-3],ifun) ) )
        if(any(x%i1 /= [-2,-4,-6]))                       error stop 85
        if(.not. associated(x%iptr))                      error stop 86
        if(any(x%iptr([-5]) /= 5))                        error stop 87
     end associate

     print *,"*****TEST 13*****"
     ! invoke assignB
     RB1=SB2(2:2)

     if(any(RB1(1)%c1 /= "CD"))                           error stop 88
     if(any(RB1(1)%acomp%i1 /= [-1,-2,-3]))               error stop 89
     if(.not. associated(RB1(1)%acomp%iptr))              error stop 90
     if(.not. associated(RB1(1)%aptr))                    error stop 91
     if(any(RB1(1)%acomp%iptr(RB1(1)%acomp%i1) /= &
                   [1,2,3] ))                             error stop 92

     associate(x=>RB1(1)%aptr(OA1(2)) )
        if(any(x%i1 /= [2,4,6]))                          error stop 93
        if(.not. associated(x%iptr,ifun))                 error stop 94
     end associate

     print *,"*****TEST 14*****"
     ! invoke assignB
     RB1(1)=SB2(1)

     if(any(RB1(1)%c1 /= "cd"))                           error stop 95
     if(any(RB1(1)%acomp%i1 /= [-1,-2,-3]))               error stop 96
     if(.not. associated(RB1(1)%acomp%iptr))              error stop 97
     if(.not. associated(RB1(1)%aptr))                    error stop 98
     if(any(RB1(1)%acomp%iptr([3,4]) /= &
                   [-3,-4] ))                             error stop 99

     associate(x=>RB1(1)%aptr(YA(4,3)([-1,-2,-3],ifun) ) )
        if(any(x%i1 /= [-2,-4,-6]))                       error stop 100
        if(.not. associated(x%iptr))                      error stop 101
        if(any(x%iptr([-5]) /= 5))                        error stop 102
     end associate

end program
