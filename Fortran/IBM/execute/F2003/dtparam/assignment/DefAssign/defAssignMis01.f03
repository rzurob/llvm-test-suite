!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 20 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test defined assignment with interface block
!* 2. Dummy argument in defined assignment procedure is allocatable or pointer with deferred length parameter
!* 3. RHS has vector subscripts
!234567490123456749012345674901234567490123456749012345674901234567490
module m
    type A(l1)
       integer,len  :: l1 ! l1=2
       character(2) :: c1(l1)
       integer      :: i1(l1)
       logical,pointer :: g1(:)
    end type

    type B(l2)
       integer,len :: l2 ! l2=1

       integer,allocatable :: i2(:)
       type(A(l2+1)),allocatable :: a1comp(:)
       type(A(:)),pointer  :: a2comp
    end type

    interface assignment(=)
       module procedure assignA1,assignA2,assignB1,assignB2
    end interface

    contains

      subroutine assignA1(this,dt)
         type(A(:)),allocatable,intent(inout) :: this(:)
         type(A(*)),intent(in) :: dt(:)
         integer :: i

         print *," in assignA1"
         if(allocated(this))     deallocate(this)

         allocate(A(2) :: this(size(dt)))

         do i=1,size(dt)
            this(i)=dt(i) ! invoke intrinsic assignment
         end do

      end subroutine

      subroutine assignA2(this,dt)
         type(A(:)),pointer,intent(inout) :: this
         type(A(*)),target,intent(in) :: dt

         print *," in assignA2"

         this=>dt !invoke pointer assignment
      end subroutine

      subroutine assignB1(this,dt)
         type(B(:)),pointer,intent(inout) :: this(:)
         type(B(*)),target,intent(in)     :: dt(:)

         print *," in assignB1"

         this=>dt !invoke pointer assignment

      end subroutine

      subroutine assignB2(this,dt)
         type(B(:)),pointer,intent(inout) :: this
         type(B(*)),target,intent(in) :: dt

         print *," in assignB2"

         allocate(B(dt%l2) :: this)

         this%i2=dt%i2
         ! this%a1comp has non-deferred length type parameter, interface mismatches assignA1,invoke intrinsic assignment instead
         this%a1comp=dt%a1comp ! invoke intrinsic assignment
         this%a2comp=dt%a2comp ! invoke assignA2

      end subroutine

end module

program defAssignMis01
     use m
     implicit none

     integer :: vec(4)=(/3,1,4,2/)

     logical,target :: g1(6)=[.true.,.false.,.false.,.true.,.false.,.true.]

     type(A(:)),pointer :: a1

     type(A(:)),allocatable,target :: a2(:),a3(:)

     type(B(:)),pointer :: b1,b2(:), b22(:)

     type(B(:)),allocatable,target :: b3(:)

     print *,"****TEST  1****"
     !invoke assignA1
     a2=[A(2)(["ab","AB"],[1,2],g1(1:3)), &
         A(2)(["cd","CD"],[3,4],g1(5:6)), &
         A(2)(["ef","EF"],[5,6],g1(2:5)), &
         A(2)(["gh","GH"],[7,8],g1(6:6))]

     if(size(a2) /= 4)                                         error stop 10
     if(any(a2(1)%c1 /= ["ab","AB"]))                          error stop 11
     if(any(a2(2)%c1 /= ["cd","CD"]))                          error stop 12
     if(any(a2(3)%c1 /= ["ef","EF"]))                          error stop 13
     if(any(a2(4)%c1 /= ["gh","GH"]))                          error stop 14

     if(any(a2(1)%i1 /= [1,2]))                                error stop 15
     if(any(a2(2)%i1 /= [3,4]))                                error stop 16
     if(any(a2(3)%i1 /= [5,6]))                                error stop 17
     if(any(a2(4)%i1 /= [7,8]))                                error stop 18

     if(any(a2(1)%g1 .neqv. [.true.,.false.,.false.]))         error stop 19
     if(any(a2(2)%g1 .neqv. [.false.,.true.]))                 error stop 20
     if(any(a2(3)%g1 .neqv. [.false.,.false.,.true.,.false.])) error stop 21
     if(any(a2(4)%g1 .neqv. [.true.]))                         error stop 22

     print *,"****TEST  2****"
     !invoke assignA2
     a1=a2(2)

     if(any(a1%c1 /= ["cd","CD"]))                              error stop 23
     if(any(a1%i1 /= [3,4]))                                    error stop 24
     if(any(a1%g1 .neqv. [.false.,.true.]))                     error stop 25

     print *,"****TEST  3****"
     !invoke assignA1
     a3=a2(vec) !RHS has vector subscripts

     if(size(a3) /= 4)                                          error stop 26
     if(any(a3(2)%c1 /= ["ab","AB"]))                           error stop 27
     if(any(a3(4)%c1 /= ["cd","CD"]))                           error stop 28
     if(any(a3(1)%c1 /= ["ef","EF"]))                           error stop 29
     if(any(a3(3)%c1 /= ["gh","GH"]))                           error stop 30

     if(any(a3(2)%i1 /= [1,2]))                                 error stop 31
     if(any(a3(4)%i1 /= [3,4]))                                 error stop 32
     if(any(a3(1)%i1 /= [5,6]))                                 error stop 33
     if(any(a3(3)%i1 /= [7,8]))                                 error stop 34

     if(any(a3(2)%g1 .neqv. [.true.,.false.,.false.]))          error stop 35
     if(any(a3(4)%g1 .neqv. [.false.,.true.]))                  error stop 36
     if(any(a3(1)%g1 .neqv. [.false.,.false.,.true.,.false.]))  error stop 37
     if(any(a3(3)%g1 .neqv. [.true.]))                          error stop 38

      print *,"****TEST  4****"
      ! invoke assignB1
      allocate (b22(4), source= &
        [B(1)([-1,-2,-3],a2(1:4:2),a1) , &
         B(1)([-4,-5],a2(2:4:2),a2(1)), &
         B(1)([-6],a2(4:4),a1),         &
         B(1)([-7,-8,-9],a2(1:1),a2(4))])

     b2 = b22
     if(size(b2) /= 4)                                          error stop 39
     if(any(a3(2)%c1 /= ["ab","AB"]))                           error stop 40
     if(any(a3(4)%c1 /= ["cd","CD"]))                           error stop 41
     if(any(a3(1)%c1 /= ["ef","EF"]))                           error stop 42
     if(any(a3(3)%c1 /= ["gh","GH"]))                           error stop 43

     if(any(a3(2)%i1 /= [1,2]))                                 error stop 44
     if(any(a3(4)%i1 /= [3,4]))                                 error stop 45
     if(any(a3(1)%i1 /= [5,6]))                                 error stop 46
     if(any(a3(3)%i1 /= [7,8]))                                 error stop 47

     if(any(a3(2)%g1 .neqv. [.true.,.false.,.false.]))          error stop 48
     if(any(a3(4)%g1 .neqv. [.false.,.true.]))                  error stop 49
     if(any(a3(1)%g1 .neqv. [.false.,.false.,.true.,.false.]))  error stop 50
     if(any(a3(3)%g1 .neqv. [.true.]))                          error stop 51

     print *,"****TEST  5****"
      !no interface matches, invoke intrinsic assignment
      b3=[B(1)([-1,-2,-3],a2(1:4:2),a1) , &
          B(1)([-4,-5],a2(2:4:2),a2(1)), &
          B(1)([-6],a2(4:4),a1),         &
          B(1)([-7,-8,-9],a2(1:1),a2(4))]

     if(size(b3) /= 4)                                           error stop 52
     if(any(b3(1)%i2 /= [-1,-2,-3]))                             error stop 53
     if(any(b3(2)%i2 /= [-4,-5]))                                error stop 54
     if(any(b3(3)%i2 /= [-6]))                                   error stop 55
     if(any(b3(4)%i2 /= [-7,-8,-9]))                             error stop 56

     if(size(b3(1)%a1comp) /= 2)                                 error stop 57
     if(size(b3(2)%a1comp) /= 2)                                 error stop 58
     if(size(b3(3)%a1comp) /= 1)                                 error stop 59
     if(size(b3(4)%a1comp) /= 1)                                 error stop 60

     if(any(b3(1)%a1comp(1)%c1 /= ["ab","AB"]))                  error stop 61
     if(any(b3(1)%a1comp(1)%i1 /= [1,2]))                        error stop 62
     if(any(b3(1)%a1comp(1)%g1 .neqv. [.true.,.false.,.false.])) error stop 63

     if(any(b3(1)%a1comp(2)%c1 /= ["ef","EF"]))                  error stop 64
     if(any(b3(1)%a1comp(2)%i1 /= [5,6]))                        error stop 65
     if(any(b3(1)%a1comp(2)%g1 .neqv.  &
                [.false.,.false.,.true.,.false.]))               error stop 66

     if(any(b3(1)%a2comp%c1 /= ["cd","CD"]))                     error stop 67
     if(any(b3(1)%a2comp%i1 /= [3,4]))                           error stop 68
     if(any(b3(1)%a2comp%g1 .neqv. [.false.,.true.]))            error stop 69

     if(any(b3(2)%a1comp(1)%c1 /= ["cd","CD"]))                  error stop 70
     if(any(b3(2)%a1comp(1)%i1 /= [3,4]))                        error stop 71
     if(any(b3(2)%a1comp(1)%g1 .neqv. [.false.,.true.]))         error stop 72

     if(any(b3(2)%a1comp(2)%c1 /= ["gh","GH"]))                  error stop 73
     if(any(b3(2)%a1comp(2)%i1 /= [7,8]))                        error stop 74
     if(any(b3(2)%a1comp(2)%g1 .neqv. .true. ))                  error stop 75

     if(any(b3(2)%a2comp%c1 /= ["ab","AB"]))                     error stop 76
     if(any(b3(2)%a2comp%i1 /= [1,2]))                           error stop 77
     if(any(b3(2)%a2comp%g1 .neqv. [.true.,.false.,.false.]))    error stop 78

     if(any(b3(3)%a1comp(1)%c1 /= ["gh","GH"]))                  error stop 79
     if(any(b3(3)%a1comp(1)%i1 /= [7,8]))                        error stop 80
     if(any(b3(3)%a1comp(1)%g1 .neqv. [.true.]))                 error stop 81

     if(any(b3(3)%a2comp%c1 /= ["cd","CD"]))                     error stop 82
     if(any(b3(3)%a2comp%i1 /= [3,4]))                           error stop 83
     if(any(b3(3)%a2comp%g1 .neqv. [.false.,.true.]))            error stop 84

     if(any(b3(4)%a1comp(1)%c1 /= ["ab","AB"]))                  error stop 85
     if(any(b3(4)%a1comp(1)%i1 /= [1,2]))                        error stop 86
     if(any(b3(4)%a1comp(1)%g1 .neqv. [.true.,.false.,.false.])) error stop 87

     if(any(b3(4)%a2comp%c1 /= ["gh","GH"]))                     error stop 88
     if(any(b3(4)%a2comp%i1 /= [7,8]))                           error stop 89
     if(any(b3(4)%a2comp%g1 .neqv. .true.))                      error stop 90

     print *,"****TEST  6****"
!      b2=b3(vec) !<-- this call is illegal, b3(vec) will be out of scope after
!      the call to assignB1
     call foo (b3(vec)) !<-- the workaround

      print *,"****TEST  7****"
      !invoke assignB2
      b1=b3(3)

     if(any(b1%a1comp(1)%c1 /= ["gh","GH"]))                     error stop 130
     if(any(b1%a1comp(1)%i1 /= [7,8]))                           error stop 131
     if(any(b1%a1comp(1)%g1 .neqv. [.true.]))                    error stop 132

     if(any(b1%a2comp%c1 /= ["cd","CD"]))                        error stop 133
     if(any(b1%a2comp%i1 /= [3,4]))                              error stop 134
     if(any(b1%a2comp%g1 .neqv. [.false.,.true.]))               error stop 135

     contains

     subroutine foo (rhs)
     type(b(*)), target, intent(in) :: rhs(:)
     b2 => rhs

     if(size(b2) /= 4)                                           error stop 91
     if(any(b2(2)%i2 /= [-1,-2,-3]))                             error stop 92
     if(any(b2(4)%i2 /= [-4,-5]))                                error stop 93
     if(any(b2(1)%i2 /= [-6]))                                   error stop 94
     if(any(b2(3)%i2 /= [-7,-8,-9]))                             error stop 95

     if(size(b2(2)%a1comp) /= 2)                                 error stop 96
     if(size(b2(4)%a1comp) /= 2)                                 error stop 97
     if(size(b2(1)%a1comp) /= 1)                                 error stop 98
     if(size(b2(3)%a1comp) /= 1)                                 error stop 99

     if(any(b2(2)%a1comp(1)%c1 /= ["ab","AB"]))                  error stop 100
     if(any(b2(2)%a1comp(1)%i1 /= [1,2]))                        error stop 101
     if(any(b2(2)%a1comp(1)%g1 .neqv. [.true.,.false.,.false.])) error stop 102

     if(any(b2(2)%a1comp(2)%c1 /= ["ef","EF"]))                  error stop 103
     if(any(b2(2)%a1comp(2)%i1 /= [5,6]))                        error stop 104
     if(any(b2(2)%a1comp(2)%g1 .neqv.  &
                [.false.,.false.,.true.,.false.]))               error stop 105

     if(any(b2(2)%a2comp%c1 /= ["cd","CD"]))                     error stop 106
     if(any(b2(2)%a2comp%i1 /= [3,4]))                           error stop 107
     if(any(b2(2)%a2comp%g1 .neqv. [.false.,.true.]))            error stop 108

     if(any(b2(4)%a1comp(1)%c1 /= ["cd","CD"]))                  error stop 109
     if(any(b2(4)%a1comp(1)%i1 /= [3,4]))                        error stop 110
     if(any(b2(4)%a1comp(1)%g1 .neqv. [.false.,.true.]))         error stop 111

     if(any(b2(4)%a1comp(2)%c1 /= ["gh","GH"]))                  error stop 112
     if(any(b2(4)%a1comp(2)%i1 /= [7,8]))                        error stop 113
     if(any(b2(4)%a1comp(2)%g1 .neqv. .true. ))                  error stop 114

     if(any(b2(4)%a2comp%c1 /= ["ab","AB"]))                     error stop 115
     if(any(b2(4)%a2comp%i1 /= [1,2]))                           error stop 116
     if(any(b2(4)%a2comp%g1 .neqv. [.true.,.false.,.false.]))    error stop 117

     if(any(b2(1)%a1comp(1)%c1 /= ["gh","GH"]))                  error stop 118
     if(any(b2(1)%a1comp(1)%i1 /= [7,8]))                        error stop 119
     if(any(b2(1)%a1comp(1)%g1 .neqv. [.true.]))                 error stop 120

     if(any(b2(1)%a2comp%c1 /= ["cd","CD"]))                     error stop 121
     if(any(b2(1)%a2comp%i1 /= [3,4]))                           error stop 122
     if(any(b2(1)%a2comp%g1 .neqv. [.false.,.true.]))            error stop 123

     if(any(b2(3)%a1comp(1)%c1 /= ["ab","AB"]))                  error stop 124
     if(any(b2(3)%a1comp(1)%i1 /= [1,2]))                        error stop 125
     if(any(b2(3)%a1comp(1)%g1 .neqv. [.true.,.false.,.false.])) error stop 126

     if(any(b2(3)%a2comp%c1 /= ["gh","GH"]))                     error stop 127
     if(any(b2(3)%a2comp%i1 /= [7,8]))                           error stop 128
     if(any(b2(3)%a2comp%g1 .neqv. .true.))                      error stop 129

     end subroutine
end program