!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb. 12 2009
!*
!*  PRIMARY FUNCTIONS TESTED   : USER DEFINED ASSIGNMENT
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!* 1. Test defined assignment with interface block
!* 2. Derived type has pointer component
!* 3. Test link list
!* 4. Defined assignment procedure is a subroutine subprogram with entry statement
!* 5. Defined assignment procedure is also a recursive subroutine
!* 6. Rename Derived type with USE statement
!234567490123456749012345674901234567490123456749012345674901234567490
module m1
   type A(k1,l1)
      integer,kind  :: k1
      integer,len   :: l1 ! l1=4
      integer(k1)   :: i1(l1+1)=-99
      character(l1) :: c1(l1) ="****"
      logical(k1)   :: g1(l1)=.false.
   end type

end module

module m2
  use m1,XA=>A
  type B(k2,l2)
    integer,kind :: k2
    integer,len  :: l2 ! l2=3

    integer(2*k2)  :: id
    type(XA(2*k2,l2+1)) :: a1comp(3)
    type(B(k2,l2)),pointer :: next=>null()
  end type

  interface assignment(=)
     module procedure  assignA1,assignA2,assignB1,assignB2
  end interface

  contains

      subroutine assignA1(this1,ta1)
          class(XA(4,*)),intent(inout)   :: this1
          type(XA(4,*)),intent(in) :: ta1

          class(XA(2,*)),intent(inout)   :: this2
          type(XA(2,*)),intent(in) :: ta2

          print *,"in assignA1"

          this1%i1=-ta1%i1
          this1%c1=ta1%c1(ubound(ta1%c1,1):lbound(ta1%c1,1):-1)
          this1%g1=.not. ta1%g1

          goto 100
      entry assignA2(this2,ta2)

          print *,"in assignA2"

          this2%i1=-2*ta2%i1
          this2%c1=ta2%c1(:)(3:4) // ta2%c1(:)(1:2)
          this2%g1=ta2%g1 .and. .true.

100    return
      end subroutine

      function getA1(ta) Result(r)
         class(XA(4,*)),intent(in) :: ta
         type(XA(ta%k1,ta%l1))  :: r

         r=ta
      end function

      function getA2(ta) Result(r)
         class(XA(2,4)),intent(in) :: ta
         type(XA(ta%k1,ta%l1))  :: r

         r=ta
      end function

      recursive subroutine assignB2(this,dt)
        class(B(1,3)),intent(inout) :: this
        type(B(1,*)),intent(in)     :: dt

        print *,"in assignB2"

        this%id = dt%id

        do i=lbound(this%a1comp,1),ubound(this%a1comp,1)
            this%a1comp(i)=dt%a1comp(i)  !invoke assignA2
        end do

        if(associated(this%next)) nullify(this%next)

        if(associated(dt%next))  then
          allocate(this%next)
          this%next = dt%next !recursive call
        end if

     end subroutine

     recursive subroutine assignB1(this,dt)
        class(B(2,3)),intent(inout) :: this
        type(B(2,*)),intent(in)     :: dt

        print *,"in assignB1"

        this%id = dt%id

        do i=lbound(this%a1comp,1),ubound(this%a1comp,1)
            this%a1comp(i)=dt%a1comp(i)  !invoke assignA1
        end do

        if(associated(this%next)) nullify(this%next)

        if(associated(dt%next)) then
            allocate(this%next)
            this%next=dt%next  ! recursive call
        end if

     end subroutine

end module

program defAssignDataPtrComp04a
    use m2,XB=>B
    implicit none

    integer :: i

    ! won't invoke defined assignment since it is static initialization
    type(XA(4,4)) :: aobj1=XA(4,4)()
    type(XA(2,4)) :: aobj2=XA(2,4)()

    type(XB(2,3)),pointer :: bobj1

    type(XB(1,3)),pointer :: bobj2

    type(XB(2,3)),pointer :: bobj3=>null()

    type(XB(1,3)),pointer :: bobj4=>null()

    print *,"*****TEST   1*****"
    !invoke assignA1
    aobj1=XA(4,4)()

    !--- verify aobj1---!
    if(aobj1%k1 /= 4)                                        error stop 10
    if(aobj1%l1 /= 4)                                        error stop 11
    if(any(aobj1%i1 /= 99))                                  error stop 12
    if(any(aobj1%c1 /= "****"))                              error stop 13
    if(any(aobj1%g1 .neqv. .true.))                          error stop 14

    print *,"*****TEST   2*****"
    !invoke assignA2
    aobj2=XA(2,4)()

    !--- verify aobj2---!
    if(aobj2%k1 /= 2)                                        error stop 15
    if(aobj2%l1 /= 4)                                        error stop 16
    if(any(aobj2%i1 /= 198))                                 error stop 17
    if(any(aobj2%c1 /= "****"))                              error stop 18
    if(any(aobj2%g1 .neqv. .false.))                         error stop 19

    print *,"*****TEST   3*****"
    ! invoke assignA1
    aobj1=XA(4,4)([1,2,3,4,5], &
                  ["abcd","ABCD","efgh","EFGH"],&
                  [.true.,.false.,.true.,.false.])

    !--- verify aobj1---!
    if(aobj1%k1 /= 4)                                        error stop 20
    if(aobj1%l1 /= 4)                                        error stop 21
    if(any(aobj1%i1 /= [-1,-2,-3,-4,-5]))                    error stop 22
    if(any(aobj1%c1 /= ["EFGH","efgh","ABCD","abcd"]))       error stop 23
    if(any(aobj1%g1 .neqv. [.false.,.true.,.false.,.true.])) error stop 24

    print *,"*****TEST   4*****"
    ! invoke assignA2
    aobj2=XA(2,4)([11,12,13,14,15], &
                  ["test","TEST","team","TEAM"],&
                  [.true.,.false.,.false.,.true.])

    !--- verify aobj2---!
    if(aobj2%k1 /= 2)                                        error stop 25
    if(aobj2%l1 /= 4)                                        error stop 26
    if(any(aobj2%i1 /= [-22,-24,-26,-28,-30]))               error stop 27
    if(any(aobj2%c1 /= ["stte","STTE","amte","AMTE"]))       error stop 28
    if(any(aobj2%g1 .neqv. [.true.,.false.,.false.,.true.])) error stop 29

    print *,"*****TEST   5*****"
    allocate(bobj1)

    ! invoke assignB1
    bobj1=XB(2,3)(10,aobj1,null())

    !--- verify bobj1---!
    if(bobj1%id /= 10)                                       error stop 30
    if(bobj1%k2 /= 2)                                        error stop 31
    if(bobj1%l2 /= 3)                                        error stop 32

    associate(x=>bobj1%a1comp)

    if(x%k1 /= 4)                                            error stop 33
    if(x%l1 /= 4)                                            error stop 34
    if(any(x(1)%i1 /= [1,2,3,4,5]))                          error stop 35
    if(any(x(1)%c1 /= ["abcd","ABCD","efgh","EFGH"]))        error stop 36
    if(any(x(1)%g1 .neqv. [.true.,.false.,.true.,.false.]))  error stop 37

    if(any(x(2)%i1 /= [1,2,3,4,5]))                          error stop 38
    if(any(x(2)%c1 /= ["abcd","ABCD","efgh","EFGH"]))        error stop 39
    if(any(x(2)%g1 .neqv. [.true.,.false.,.true.,.false.]))  error stop 40

    end associate

    print *,"*****TEST   6*****"
    allocate(bobj2)

    ! invoke assignB2
    bobj2=XB(1,3)(50,aobj2,null())

    !--- verify bobj2---!

    if(bobj2%id /= 50)                                       error stop 41
    if(bobj2%k2 /= 1)                                        error stop 42
    if(bobj2%l2 /= 3)                                        error stop 43

    associate(x=>bobj2%a1comp)
    if(x%k1 /= 2)                                            error stop 44
    if(x%l1 /= 4)                                            error stop 45
    if(any(x(1)%i1 /= [44,48,52,56,60]))                     error stop 46
    if(any(x(1)%c1 /= ["test","TEST","team","TEAM"]))        error stop 47
    if(any(x(1)%g1 .neqv. [.true.,.false.,.false.,.true.]))  error stop 48

    if(any(x(1)%i1 /= [44,48,52,56,60]))                     error stop 49
    if(any(x(1)%c1 /= ["test","TEST","team","TEAM"]))        error stop 50
    if(any(x(1)%g1 .neqv. [.true.,.false.,.false.,.true.]))  error stop 51

    end associate

    print *,"*****TEST   7*****"
    allocate(XB(2,3) :: bobj3)

    ! invoke assignB1
    bobj3=bobj1

    !--- verify bobj3--!

    if(bobj3%id /= 10)                                       error stop 52
    if(bobj3%k2 /= 2)                                        error stop 53
    if(bobj3%l2 /= 3)                                        error stop 54

    associate(x=>bobj3%a1comp)

    if(x%k1 /= 4)                                            error stop 55
    if(x%l1 /= 4)                                            error stop 56
    if(any(x(1)%i1 /= [-1,-2,-3,-4,-5]))                     error stop 57
    if(any(x(1)%c1 /= ["EFGH","efgh","ABCD","abcd"]))        error stop 58
    if(any(x(1)%g1 .neqv. [.false.,.true.,.false.,.true.]))  error stop 59

    if(any(x(2)%i1 /= [-1,-2,-3,-4,-5]))                     error stop 60
    if(any(x(2)%c1 /= ["EFGH","efgh","ABCD","abcd"]))        error stop 61
    if(any(x(2)%g1 .neqv. [.false.,.true.,.false.,.true.]))  error stop 62

    end associate

    print *,"*****TEST   8*****"
    allocate(XB(1,3) :: bobj4)

    ! invoke assignB2
    bobj4=bobj2

    !--- verify bobj4---!

    if(bobj4%id /= 50)                                       error stop 63
    if(bobj4%k2 /= 1)                                        error stop 64
    if(bobj4%l2 /= 3)                                        error stop 65

    associate(x=>bobj4%a1comp)

    if(x%k1 /= 2)                                            error stop 66
    if(x%l1 /= 4)                                            error stop 67
    if(any(x(1)%i1 /= [-88,-96,-104,-112,-120]))             error stop 68
    if(any(x(1)%c1 /= ["stte","STTE","amte","AMTE"]))        error stop 69
    if(any(x(1)%g1 .neqv. [.true.,.false.,.false.,.true.]))  error stop 70

    if(any(x(1)%i1 /= [-88,-96,-104,-112,-120]))             error stop 71
    if(any(x(1)%c1 /= ["stte","STTE","amte","AMTE"]))        error stop 72
    if(any(x(1)%g1 .neqv. [.true.,.false.,.false.,.true.]))  error stop 73

    end associate

    print *,"*****TEST   9*****"
    allocate(bobj1%next,source=XB(2,3)(100,getA1(aobj1),null()) )
    allocate(bobj1%next%next,source=XB(2,3)(200,getA1(aobj1),null()))
    allocate(bobj1%next%next%next,source=XB(2,3)(300,getA1(aobj1),null()))

    deallocate(bobj3)

    allocate(XB(2,3) :: bobj3)

    !invoke assignB1
    bobj3=bobj1
    !--- verify bobj3--!
    if(.not. associated(bobj3))                             error stop 74
    if(.not. associated(bobj3%next))                        error stop 75
    if(.not. associated(bobj3%next%next))                   error stop 76
    if(.not. associated(bobj3%next%next%next))              error stop 77

    associate(x=>bobj3%next%next%next%a1comp)

    if(x%k1 /= 4)                                            error stop 78
    if(x%l1 /= 4)                                            error stop 79
    if(any(x(1)%i1 /= [-1,-2,-3,-4,-5]))                     error stop 80
    if(any(x(1)%c1 /= ["EFGH","efgh","ABCD","abcd"]))        error stop 81
    if(any(x(1)%g1 .neqv. [.false.,.true.,.false.,.true.]))  error stop 82

    if(any(x(2)%i1 /= [-1,-2,-3,-4,-5]))                     error stop 83
    if(any(x(2)%c1 /= ["EFGH","efgh","ABCD","abcd"]))        error stop 84
    if(any(x(2)%g1 .neqv. [.false.,.true.,.false.,.true.]))  error stop 85

    end associate

    print *,"*****TEST  10*****"
    allocate(bobj2%next,source=XB(1,3)(100,getA2(aobj2),null()) )
    allocate(bobj2%next%next,source=XB(1,3)(200,getA2(aobj2),null()))
    allocate(bobj2%next%next%next,source=XB(1,3)(300,getA2(aobj2),null()))

    deallocate(bobj4)

    allocate(bobj4)

    ! invoke assignB2
    bobj4=bobj2

    !--- verify bobj4---!

    if(.not. associated(bobj4))                             error stop 86
    if(.not. associated(bobj4%next))                        error stop 87
    if(.not. associated(bobj4%next%next))                   error stop 88
    if(.not. associated(bobj4%next%next%next))              error stop 89

    associate(x=>bobj4%next%next%next%a1comp)

    if(x%k1 /= 2)                                            error stop 90
    if(x%l1 /= 4)                                            error stop 91
    if(any(x(1)%i1 /= [-88,-96,-104,-112,-120]))             error stop 92
    if(any(x(1)%c1 /= ["stte","STTE","amte","AMTE"]))        error stop 93
    if(any(x(1)%g1 .neqv. [.true.,.false.,.false.,.true.]))  error stop 94

    if(any(x(2)%i1 /= [-88,-96,-104,-112,-120]))             error stop 95
    if(any(x(2)%c1 /= ["stte","STTE","amte","AMTE"]))        error stop 96
    if(any(x(2)%g1 .neqv. [.true.,.false.,.false.,.true.]))  error stop 97

    end associate

end program