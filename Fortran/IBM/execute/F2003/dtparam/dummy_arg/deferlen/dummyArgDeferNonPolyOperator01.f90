!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Nov. 22 2008
!*
!*  PRIMARY FUNCTIONS TESTED   : DUMMY ARGUMENT WITH DEFERRED LENGTH
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  :
!*
!*  DESCRIPTION
!*  1. allocatable variable with deferred length parameter is used as dummy argument of specific procedure.
!*  2. user defined operator is "+"
!*  3.it will invoke corresponding specific procedure when user defined "+" is used in statement.
!234567890123456789012345678901234567890123456789012345678901234567890
module m
   type dtp(k1,l1)
      integer,kind  :: k1=4
      integer,len   :: l1=3
      integer(k1)   :: i=99
      character(l1) :: c="xlf"
      contains
        procedure,pass :: add1
        procedure,pass :: add2
        procedure,pass :: add3
        generic :: operator(+) =>add1,add2,add3
   end type
   contains

     function add1(this,dt)
        class(dtp(2,*)),intent(in) :: this
        type(dtp(2,:)),allocatable,intent(in) :: dt
        type(dtp(2,:)),allocatable :: add1

        allocate(dtp(2,this%l1+dt%l1) :: add1)

        add1%i=this%i+dt%i
        add1%c=this%c//dt%c
     end function

     function add2(this,dt)
        class(dtp(2,*)),intent(in) :: this
        type(dtp(2,:)),allocatable,intent(in) :: dt(:)
        type(dtp(2,:)),allocatable :: add2(:)
        integer :: i

        allocate(dtp(2,dt%l1+this%l1) :: add2(size(dt)))
        add2%i=this%i+dt%i
        add2%c=this%c//dt%c
     end function

     function add3(this,dt)
        class(dtp(2,*)),intent(in) :: this
        type(dtp(4,:)),allocatable,intent(in) :: dt
        type(dtp(2,:)),allocatable :: add3

        allocate(dtp(2,this%l1+dt%l1) :: add3)

        add3%i=this%i+dt%i
        add3%c=this%c//dt%c
     end function

end module

program dummyArgDeferNonPolyOperator01
  use m
  implicit none

  type(dtp(2,3)) :: dtp1

  type(dtp(2,:)),allocatable :: dtp2(:),dtp3,result1(:),result2

  type(dtp(4,:)),allocatable :: tar

  dtp1=dtp(2,3)(i=10,c="abc")

  allocate(dtp2(3),source= &
    [dtp(2,5)(i=1,c="00000"),dtp(2,5)(i=2,c="11111"),dtp(2,5)(i=3,c="22222")])


  result1= dtp1 + dtp2

  if(size(result1,1) /= 3)                           error stop 10_4
  if(any(result1%i /= [11,12,13]))                   error stop 11_4
  if(any(result1%c /= ["abc00000","abc11111","abc22222"] ))   error stop 12_4

  dtp3=dtp(2,4)(-1,"1234")

  result2= dtp1 + dtp3

  if(result2%i /= 9)                                 error stop 13_4
  if(result2%c /= "abc1234")                         error stop 14_4


  tar=dtp(4,1)(i=-20,c="Q")

  deallocate(result2)

  result2=dtp1 + tar

  if(result2%i /= -10)                               error stop 15_4
  if(result2%c /= "abcQ")                            error stop 16_4

end program
